#define _GNU_SOURCE
#include <caml/alloc.h>
#include <caml/fail.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <unistd.h>

#ifndef RENAME_NOREPLACE
#define RENAME_NOREPLACE (1 << 0)
#endif

static void fail_errno(const char *op) {
  char buf[256];
  snprintf(buf, sizeof(buf), "%s: %s", op, strerror(errno));
  caml_failwith(buf);
}

static int unsafe_component(const char *s) {
  return !*s || !strcmp(s, ".") || !strcmp(s, "..");
}

static int open_dir_chain(const char *path) {
  char *copy = strdup(path), *save = NULL, *part;
  int fd, next;
  if (!copy) fail_errno("strdup");
  if (!strcmp(path, ".")) {
    free(copy);
    fd = open(".", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (fd < 0) fail_errno("open starting directory");
    return fd;
  }
  if (strstr(path, "//") || strchr(path, '\\')) { free(copy); errno = EINVAL; fail_errno("unsafe path"); }
  fd = open(path[0] == '/' ? "/" : ".", O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
  if (fd < 0) { free(copy); fail_errno("open root directory"); }
  part = strtok_r(copy, "/", &save);
  while (part) {
    if (unsafe_component(part)) { close(fd); free(copy); errno = EINVAL; fail_errno("unsafe path component"); }
    next = openat(fd, part, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (next < 0) { close(fd); free(copy); fail_errno("openat directory"); }
    close(fd); fd = next; part = strtok_r(NULL, "/", &save);
  }
  free(copy); return fd;
}

static int open_parent_at(int rootfd, const char *relative, char **name_out) {
  char *copy = strdup(relative), *save = NULL, *part, *nextpart;
  int fd = dup(rootfd), next;
  if (!copy || fd < 0) fail_errno("prepare relative path");
  if (relative[0] == '/' || strstr(relative, "//") || strchr(relative, '\\')) {
    close(fd); free(copy); errno = EINVAL; fail_errno("unsafe relative path");
  }
  part = strtok_r(copy, "/", &save);
  if (!part) { close(fd); free(copy); errno = EINVAL; fail_errno("empty relative path"); }
  while ((nextpart = strtok_r(NULL, "/", &save)) != NULL) {
    if (unsafe_component(part)) { close(fd); free(copy); errno = EINVAL; fail_errno("unsafe path component"); }
    next = openat(fd, part, O_RDONLY | O_DIRECTORY | O_NOFOLLOW | O_CLOEXEC);
    if (next < 0) { close(fd); free(copy); fail_errno("openat parent"); }
    close(fd); fd = next; part = nextpart;
  }
  if (unsafe_component(part)) { close(fd); free(copy); errno = EINVAL; fail_errno("unsafe filename"); }
  *name_out = strdup(part); free(copy);
  if (!*name_out) { close(fd); fail_errno("strdup filename"); }
  return fd;
}

static char *read_fd_all(int fd, size_t *len_out) {
  struct stat st; char *buf; size_t off = 0; ssize_t n;
  if (fstat(fd, &st) || !S_ISREG(st.st_mode)) { errno = EINVAL; fail_errno("regular file required"); }
  if (st.st_size < 0 || st.st_size > 64 * 1024 * 1024) { errno = EFBIG; fail_errno("file too large"); }
  buf = malloc((size_t)st.st_size + 1); if (!buf) fail_errno("malloc");
  while (off < (size_t)st.st_size) {
    n = read(fd, buf + off, (size_t)st.st_size - off);
    if (n < 0) { free(buf); fail_errno("read"); }
    if (!n) break;
    off += (size_t)n;
  }
  buf[off] = 0; *len_out = off; return buf;
}

CAMLprim value cwr_secure_read_regular(value vpath) {
  CAMLparam1(vpath); CAMLlocal1(out);
  const char *path = String_val(vpath); char *copy = strdup(path), *slash, *name;
  int parent, fd; size_t len; char *buf;
  if (!copy) fail_errno("strdup");
  slash = strrchr(copy, '/');
  if (slash) { *slash = 0; name = slash + 1; parent = open_dir_chain(*copy ? copy : "/"); }
  else { name = copy; parent = open_dir_chain("."); }
  if (unsafe_component(name)) { close(parent); free(copy); errno = EINVAL; fail_errno("unsafe filename"); }
  fd = openat(parent, name, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
  if (fd < 0) { close(parent); free(copy); fail_errno("openat regular file"); }
  buf = read_fd_all(fd, &len); close(fd); close(parent); free(copy);
  out = caml_alloc_initialized_string(len, buf); free(buf); CAMLreturn(out);
}

CAMLprim value cwr_secure_write_atomic(value vroot, value vrel, value vcontent) {
  CAMLparam3(vroot, vrel, vcontent);
  const char *root = String_val(vroot), *rel = String_val(vrel), *content = String_val(vcontent);
  size_t len = caml_string_length(vcontent), off = 0, oldlen; ssize_t n;
  int rootfd = -1, parent = -1, checkroot = -1, checkparent = -1, tmpfd = -1, targetfd = -1;
  char *name = NULL, *checkname = NULL, tmp[NAME_MAX]; struct stat a, b; char *old = NULL;
  rootfd = open_dir_chain(root); parent = open_parent_at(rootfd, rel, &name);
  targetfd = openat(parent, name, O_RDONLY | O_NOFOLLOW | O_CLOEXEC);
  if (targetfd >= 0) {
    old = read_fd_all(targetfd, &oldlen);
    if (oldlen == len && !memcmp(old, content, len)) { close(targetfd); free(old); free(name); close(parent); close(rootfd); CAMLreturn(Val_int(0)); }
    close(targetfd); free(old); errno = EEXIST;
    fail_errno("conflicting pre-existing artifact");
  } else if (errno != ENOENT) fail_errno("inspect artifact target");
  snprintf(tmp, sizeof(tmp), "%s.cwr-%ld-%ld.tmp", name, (long)getpid(), (long)random());
  tmpfd = openat(parent, tmp, O_WRONLY | O_CREAT | O_EXCL | O_NOFOLLOW | O_CLOEXEC, 0600);
  if (tmpfd < 0) fail_errno("create temporary artifact");
  while (off < len) { n = write(tmpfd, content + off, len - off); if (n <= 0) fail_errno("write artifact"); off += (size_t)n; }
  if (fsync(tmpfd)) fail_errno("fsync artifact");
  close(tmpfd); tmpfd = -1;
  if (fstatat(parent, name, &a, AT_SYMLINK_NOFOLLOW) == 0 || errno != ENOENT) {
    unlinkat(parent, tmp, 0); errno = EEXIST; fail_errno("artifact appeared concurrently");
  }
  checkroot = open_dir_chain(root); checkparent = open_parent_at(checkroot, rel, &checkname);
  if (strcmp(name, checkname) || fstat(parent, &a) || fstat(checkparent, &b) ||
      a.st_dev != b.st_dev || a.st_ino != b.st_ino) { unlinkat(parent, tmp, 0); errno = ESTALE; fail_errno("artifact parent changed concurrently"); }
  close(checkparent); close(checkroot); free(checkname);
#ifdef SYS_renameat2
  if (syscall(SYS_renameat2, parent, tmp, parent, name, RENAME_NOREPLACE)) {
    unlinkat(parent, tmp, 0); fail_errno("renameat2 artifact");
  }
#else
  unlinkat(parent, tmp, 0); errno = ENOSYS; fail_errno("renameat2 unavailable");
#endif
  checkroot = open_dir_chain(root); checkparent = open_parent_at(checkroot, rel, &checkname);
  if (strcmp(name, checkname) || fstat(parent, &a) || fstat(checkparent, &b) ||
      a.st_dev != b.st_dev || a.st_ino != b.st_ino) {
    free(checkname); close(checkparent); close(checkroot);
    free(name); close(parent); close(rootfd); CAMLreturn(Val_int(1));
  }
  free(checkname); close(checkparent); close(checkroot);
  const char *inject_fsync = getenv("CWR_TEST_FAIL_DIR_FSYNC");
  if ((inject_fsync && *inject_fsync) || fsync(parent)) {
    free(name); close(parent); close(rootfd); CAMLreturn(Val_int(1));
  }
  free(name); close(parent); close(rootfd); CAMLreturn(Val_int(0));
}
