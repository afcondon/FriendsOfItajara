// Read one query parameter, then remove it from the address without a reload.
export const takeParam = (key) => () => {
  const url = new URL(window.location.href);
  const v = url.searchParams.get(key) || "";
  if (url.searchParams.has(key)) {
    url.searchParams.delete(key);
    window.history.replaceState(null, "", url.pathname + url.search + url.hash);
  }
  return v;
};
