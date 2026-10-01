// Quadrat on the Atlantis tab bus. The protocol is Binnacle.TabBus's: JSON on
// a BroadcastChannel named "atlantis". Quadrat does not depend on Binnacle
// (it keeps to the Itajara client, so it can leave the house), so this is
// the few lines of the protocol it needs, and no more.
const SLOT = "quadrat";
const channel = typeof BroadcastChannel === "undefined" ? null : new BroadcastChannel("atlantis");
let armed = false;

const post = (t, playing) => {
  if (channel) channel.postMessage(JSON.stringify({ t, machine: SLOT, alias: null, edited: false, playing, root: null, offsets: null }));
};

// Start announcing: every 1.5 s as the other pages do, at once when asked
// ("hello"), and "bye" when the page goes. Play, stop and panic are not
// obeyed: Quadrat's "playing" is a capture being armed, and a dashboard must
// not start a take or cut one short.
export const start = () => {
  if (!channel) return;
  channel.addEventListener("message", (e) => {
    try { if (JSON.parse(e.data).t === "hello") post("state", armed); } catch (_) {}
  });
  setInterval(() => post("state", armed), 1500);
  window.addEventListener("pagehide", () => post("bye", false));
  post("state", armed);
};

// Whether a capture is armed. Posted at once when it changes.
export const setArmed = (on) => () => {
  if (on !== armed) { armed = on; post("state", armed); }
};
