// Holds the Friends server, as a WRITER, to what es9-daemon reads
// (binnacle/conformance/ES9-OSC.md).
//
// The real server.mjs is started on a spare port with ES9_DAEMON_ADDR aimed
// at a socket opened here. For every declared packet at an address the
// server sends, the same values are POSTed to /api/cv, and what arrives on
// the socket must be the declared bytes. Nothing reaches the real
// es9-daemon, and the running server is not touched.
//
// Run: node conformance/wire.mjs   (from friends-of-itajara/)
// The vector file is found in $BINNACLE_DIR, or in the sibling checkout.
// Not finding it is a failure, not a skip.

import assert from "node:assert/strict";
import { spawn } from "node:child_process";
import dgram from "node:dgram";
import { readFileSync } from "node:fs";
import { join } from "node:path";
import { fileURLToPath } from "node:url";

const here = (path) => fileURLToPath(new URL(path, import.meta.url));
const binnacle = process.env.BINNACLE_DIR || here("../../live-coding/binnacle");
const vectors = JSON.parse(readFileSync(join(binnacle, "conformance/vectors/es9-osc.json"), "utf8"));
assert.equal(vectors["es9-osc-vectors"], 1, "a vector format this replayer understands");
assert.ok(vectors.packets.length > 0, "no packets: nothing would be tested");

const HTTP_PORT = 39329;
const sleep = (ms) => new Promise((resolve) => setTimeout(resolve, ms));

// What arrives, in order.
const arrived = [];
let waiting = null;
const listener = dgram.createSocket("udp4");
listener.on("message", (packet) => {
  arrived.push(packet.toString("hex"));
  if (waiting) waiting();
});
await new Promise((resolve) => listener.bind(0, "127.0.0.1", resolve));

const next = () =>
  new Promise((resolve, reject) => {
    if (arrived.length) return resolve(arrived.shift());
    const timer = setTimeout(() => reject(new Error("nothing arrived")), 1500);
    waiting = () => {
      clearTimeout(timer);
      waiting = null;
      resolve(arrived.shift());
    };
  });

const server = spawn(process.execPath, [here("../server.mjs")], {
  env: { ...process.env, PORT: String(HTTP_PORT), ES9_DAEMON_ADDR: `127.0.0.1:${listener.address().port}` },
  stdio: "ignore",
});

const post = async (body) => {
  const reply = await fetch(`http://127.0.0.1:${HTTP_PORT}/api/cv`, {
    method: "POST",
    headers: { "content-type": "application/json" },
    body: JSON.stringify(body),
  });
  return reply.json();
};

// How each declared address is asked for through /api/cv.
const ask = {
  "/cv": ([bus, level]) => ({ set: [{ bus, level }] }),
  "/esx": ([slot, level]) => ({ esx: [{ slot, level }] }),
  "/cv/trig": ([bus, level, ms]) => ({ pulse: { bus, level, ms } }),
  "/cv/trig/at": ([bus, level, ms, delayMs]) => ({ pulseAt: { bus, level, ms, delayMs } }),
};

const failures = [];
const exercised = new Set();
try {
  for (let tries = 0; ; tries++) {
    try {
      await fetch(`http://127.0.0.1:${HTTP_PORT}/`);
      break;
    } catch {
      if (tries > 100) throw new Error("the server did not start");
      await sleep(30);
    }
  }

  for (const packet of vectors.packets) {
    const body = ask[packet.address];
    if (!body) continue; // an address this server does not send, or /esx5gate below
    exercised.add(packet.address);
    await post(body(packet.values));
    const got = await next().catch((error) => error.message);
    if (got !== packet.hex) failures.push(`${packet.name}: sent ${got}, declared ${packet.hex}`);
  }

  // An ES-5 gate is sent high, then low after a hold: one request, two
  // packets, which must be the declared high and low for that gate.
  const high = vectors.packets.find((p) => p.address === "/esx5gate" && p.values[1] === 1);
  const low = vectors.packets.find((p) => p.address === "/esx5gate" && p.values[1] === 0 && p.values[0] === high?.values[0]);
  if (!high || !low) failures.push("the vectors have no /esx5gate high and low for one gate");
  else {
    exercised.add("/esx5gate");
    await post({ es5pulse: { bit: high.values[0], ms: 20 } });
    for (const packet of [high, low]) {
      const got = await next().catch((error) => error.message);
      if (got !== packet.hex) failures.push(`${packet.name}: sent ${got}, declared ${packet.hex}`);
    }
  }

  await sleep(50);
  if (arrived.length) failures.push(`${arrived.length} packets arrived that no declared packet accounts for`);
  for (const address of [...Object.keys(ask), "/esx5gate"])
    if (!exercised.has(address)) failures.push(`no declared packet exercises ${address}, which the server sends`);
} catch (error) {
  failures.push(error.message);
} finally {
  server.kill();
  listener.close();
}

for (const failure of failures) console.log(`FAIL  ${failure}`);
console.log(`friends server as a writer: ${exercised.size} addresses; ${failures.length} failures`);
process.exit(failures.length ? 1 : 0);
