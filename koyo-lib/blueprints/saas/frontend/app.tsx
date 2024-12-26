import "@fontsource/outfit";
import * as React from "react";
import { createRoot } from "react-dom/client";
import { Provider as ReduxProvider } from "react-redux";
import { RouterProvider } from "react-router";

import "./app.css";
import { Provider } from "./components/chakra/provider";
import { router } from "./router";
import { initStore, store } from "./store";

const el = document.querySelector("#app") as HTMLElement;
const root = createRoot(el);
initStore();

let app = (
  <ReduxProvider store={store}>
    <Provider>
      <RouterProvider router={router} />
    </Provider>
  </ReduxProvider>
);
if ((window as any).ENVIRONMENT === "dev") {
  app = <React.StrictMode>{app}</React.StrictMode>;
}
root.render(app);
