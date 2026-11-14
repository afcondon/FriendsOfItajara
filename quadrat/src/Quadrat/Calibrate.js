// The Calibration page's three reaches outside PureScript: asking the server
// to read pitches from a take, the time, and playing a take with a reference
// tone beside it.

// The pitch of each region of a take, read `readings` times, through the
// server's /api/onsets (which runs `msm onset --regions - --pitch N`).
export const measureImpl = (take) => (regions) => (readings) => () =>
  fetch("/api/onsets", {
    method: "POST",
    headers: { "Content-Type": "application/json" },
    body: JSON.stringify({ take, regions, pitch: readings }),
  })
    .then((r) => r.json())
    .then((d) => ({
      ok: d.ok === true,
      output: typeof d.output === "string" ? d.output : "",
      regions: Array.isArray(d.regions)
        ? d.regions.map((region) => ({
            rms: Number(region.rms) || 0,
            pitch: Array.isArray(region.pitch)
              ? region.pitch.map((p) => (p && isFinite(p.hz) ? { hz: p.hz, clarity: p.clarity, periods: p.periods } : null))
              : [],
          }))
        : [],
    }))
    .catch((e) => ({ ok: false, output: String(e && e.message ? e.message : e), regions: [] }));

export const nowIso = () => new Date().toISOString();

// One player at a time: a take through an <audio> element and, when asked,
// a sine at each step's target pitch, started and stopped along the take's
// own times. A voice a cent off the reference beats slowly against it, which
// the ear hears without having to judge pitch.
let playing = null;

export const stopImpl = () => {
  if (!playing) return;
  try { playing.audio.pause(); } catch (e) {}
  try { playing.context && playing.context.close(); } catch (e) {}
  playing = null;
};

export const playImpl = (take) => (tones) => (withTone) => () => {
  stopImpl();
  const audio = new Audio("/api/take-audio?take=" + encodeURIComponent(take));
  let context = null;
  if (withTone && tones.length) {
    context = new (window.AudioContext || window.webkitAudioContext)();
    const start = context.currentTime + 0.15;
    for (const tone of tones) {
      const oscillator = context.createOscillator();
      const gain = context.createGain();
      oscillator.type = "sine";
      oscillator.frequency.value = tone.hz;
      // A short fade at each end, so a step is not a click.
      gain.gain.setValueAtTime(0, start + tone.at);
      gain.gain.linearRampToValueAtTime(0.12, start + tone.at + 0.03);
      gain.gain.setValueAtTime(0.12, start + tone.at + Math.max(0.05, tone.secs - 0.03));
      gain.gain.linearRampToValueAtTime(0, start + tone.at + tone.secs);
      oscillator.connect(gain).connect(context.destination);
      oscillator.start(start + tone.at);
      oscillator.stop(start + tone.at + tone.secs + 0.02);
    }
    setTimeout(() => audio.play().catch(() => {}), 150);
  } else {
    audio.play().catch(() => {});
  }
  audio.addEventListener("ended", () => { if (playing && playing.audio === audio) stopImpl(); });
  playing = { audio, context };
};
