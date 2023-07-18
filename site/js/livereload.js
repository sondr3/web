(() => {
  const url = "/sse";
  let sse = null;

  const connect = () => {
    sse = new EventSource(url);

    sse.addEventListener("reload", (_event) => {
      console.info("Reloading page");
      location.reload();
    });

    sse.addEventListener("shutdown", (_event) => {
      sse.close();
      setTimeout(connect, 2000);
    })

    sse.addEventListener("open", (_event) => {
      console.log(`Socket connected`);
    });

    sse.addEventListener("close", (e) => {
      console.log(`Socket closed, attempting to reconnect: ${e.reason}`);
      sse = null;

      setTimeout(connect, 2000);
    });

    sse.addEventListener("error", (e) => {
      console.error(`Socket error, closing: ${e.message}`);
      sse.close();
    });
  };

  connect();
})();