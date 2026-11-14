// Where Amphora is: the page's own host, on :3024 (it answers any origin).
export const storeBase = () => {
  const host = typeof window !== "undefined" && window.location && window.location.hostname ? window.location.hostname : "localhost";
  const proto = typeof window !== "undefined" && window.location && window.location.protocol === "https:" ? "https:" : "http:";
  return proto + "//" + host + ":3024";
};
