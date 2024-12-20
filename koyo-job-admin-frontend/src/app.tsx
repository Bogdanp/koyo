import "@fontsource/outfit";
import * as React from "react";
import { createRoot } from "react-dom/client";
import {
  Route,
  RouterProvider,
  createBrowserRouter,
  createRoutesFromElements,
} from "react-router";

import { getRoot, setRoot } from "./api";
import "./app.css";
import { DashboardPage, loadDashboardPage } from "./components/dashboard-page";
import { JobPage, loadJobPage } from "./components/job-page";
import { Provider } from "./components/ui/provider";

const el = document.querySelector("#app") as HTMLElement;
setRoot(el.dataset.root);

const root = createRoot(el);
const router = createBrowserRouter(
  createRoutesFromElements(
    <Route>
      <Route index loader={loadDashboardPage} element={<DashboardPage />} />
      <Route path="jobs">
        <Route path=":id" loader={loadJobPage} element={<JobPage />} />
      </Route>
    </Route>,
  ),
  { basename: getRoot() },
);

root.render(
  <Provider>
    <RouterProvider router={router} />
  </Provider>,
);
