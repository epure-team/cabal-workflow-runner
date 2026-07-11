#!/usr/bin/env node
import { readFileSync } from "node:fs";
import { createPublicKey, verify } from "node:crypto";

function restricted(value) {
  if (value === null || typeof value === "string" || typeof value === "boolean") return;
  if (typeof value === "number") {
    if (!Number.isSafeInteger(value)) throw new Error("non-canonical number");
    return;
  }
  if (Array.isArray(value)) return value.forEach(restricted);
  if (typeof value === "object") return Object.values(value).forEach(restricted);
  throw new Error("non-JSON value");
}

function canonical(value) {
  restricted(value);
  if (Array.isArray(value)) return `[${value.map(canonical).join(",")}]`;
  if (value !== null && typeof value === "object") {
    const keys = Object.keys(value).sort((a, b) =>
      Buffer.compare(Buffer.from(a, "utf8"), Buffer.from(b, "utf8")));
    return `{${keys.map(k => `${JSON.stringify(k)}:${canonical(value[k])}`).join(",")}}`;
  }
  return JSON.stringify(value);
}

if (process.argv[2] === "--self-test") {
  const vector = { "𐀀": 2, "": 1 };
  if (canonical(vector) !== '{"":1,"𐀀":2}')
    throw new Error("UTF-8 byte-order canonical vector mismatch");
  restricted(9007199254740991);
  restricted(-9007199254740991);
  for (const bad of [9007199254740992, -9007199254740992]) {
    let rejected = false;
    try { restricted(bad); } catch { rejected = true; }
    if (!rejected) throw new Error("unsafe integer vector accepted");
  }
  console.log("canonical cross-runtime vectors: OK");
  process.exit(0);
}

const path = process.argv[2];
const args = new Map();
for (let i = 3; i < process.argv.length; i += 2) args.set(process.argv[i], process.argv[i + 1]);
const required = ["--public-identity", "--workflow-digest", "--step", "--domain",
  "--session", "--occurrence", "--output-path", "--selected-json"];
if (!path || required.some(k => !args.has(k)))
  throw new Error("usage: verify-attestation.mjs ARTIFACT --public-identity FILE --workflow-digest DIGEST --step ID --domain DOMAIN --session NONCE --occurrence N --output-path PATH --selected-json JSON");
const raw = readFileSync(path, "utf8").trim();
const envelope = JSON.parse(raw);
if (canonical(envelope) !== raw) throw new Error("artifact is not canonical restricted JSON");
const { signature, ...unsigned } = envelope;
if (typeof signature !== "string" || unsigned.algorithm !== "Ed25519")
  throw new Error("invalid envelope");
const identityRaw = readFileSync(args.get("--public-identity"), "utf8").trim();
const identity = JSON.parse(identityRaw);
if (canonical(identity) !== identityRaw) throw new Error("public identity is not canonical");
if (identity.algorithm !== "Ed25519" || identity.key_id !== unsigned.key_id ||
    identity.public_key !== unsigned.public_key) throw new Error("public identity pin mismatch");
const occurrence = Number(args.get("--occurrence"));
if (!Number.isSafeInteger(occurrence) || occurrence < 0) throw new Error("invalid expected occurrence");
const selectedRaw = args.get("--selected-json");
const selected = JSON.parse(selectedRaw);
restricted(selected);
if (canonical(selected) !== selectedRaw)
  throw new Error("expected selected JSON is not canonical restricted JSON");
const payload = unsigned.payload;
if (payload.workflow?.digest !== args.get("--workflow-digest") ||
    payload.step_id !== args.get("--step") ||
    payload.replay_domain !== args.get("--domain") ||
    payload.session_nonce !== args.get("--session") ||
    payload.occurrence !== occurrence ||
    payload.output_path !== args.get("--output-path") ||
    canonical(payload.selected) !== canonical(selected))
  throw new Error("expected attestation binding mismatch");
const rawPublic = Buffer.from(identity.public_key, "base64");
const spkiPrefix = Buffer.from("302a300506032b6570032100", "hex");
const key = createPublicKey({ key: Buffer.concat([spkiPrefix, rawPublic]), format: "der", type: "spki" });
if (!verify(null, Buffer.from(canonical(unsigned)), key, Buffer.from(signature, "base64")))
  throw new Error("invalid signature");
console.log(`VALID PINNED ED25519 ATTESTATION: ${unsigned.key_id}`);
