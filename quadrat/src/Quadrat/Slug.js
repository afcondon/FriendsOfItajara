// The clock, and nothing else. Formatting lives on the PureScript side so the
// shape of a take's name is one thing in one place.
export const _stamp = () => {
  const d = new Date();
  const p = (n) => String(n).padStart(2, "0");
  return (
    p(d.getMonth() + 1) + p(d.getDate()) + "-" +
    p(d.getHours()) + p(d.getMinutes()) + p(d.getSeconds())
  );
};
