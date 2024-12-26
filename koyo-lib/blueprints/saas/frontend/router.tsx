import * as React from "react";
import {
  Route,
  createBrowserRouter,
  createRoutesFromElements,
} from "react-router";

import { UserGate } from "./gates/user";
import { WorkspaceGate } from "./gates/workspace";
import { DashboardPage } from "./pages/dashboard";
import { ErrorPage } from "./pages/error";
import { GeneralSettingsPage } from "./pages/general-settings";
import {
  JoinWorkspacePage,
  loadJoinWorkspacePage,
} from "./pages/join-workspace";
import { LoginPage } from "./pages/login";
import { MemberSettingsPage } from "./pages/member-settings";
import { NewWorkspacePage } from "./pages/new-workspace";
import { ResetPasswordPage } from "./pages/reset-password";
import { ResetPasswordRequestPage } from "./pages/reset-password-request";
import { SignUpPage } from "./pages/sign-up";
import { WorkspacesPage } from "./pages/workspaces";

export const router = createBrowserRouter(
  createRoutesFromElements(
    <Route errorElement={<ErrorPage />}>
      <Route index element={<UserGate navigateTo="workspaces" />} />
      <Route path="login" element={<LoginPage />} />
      <Route path="sign-up" element={<SignUpPage />} />
      <Route path="reset-password">
        <Route index element={<ResetPasswordRequestPage />} />
        <Route path=":uid/:token" element={<ResetPasswordPage />} />
      </Route>
      <Route path="workspaces" element={<UserGate />}>
        <Route index element={<WorkspacesPage />} />
        <Route path="new" element={<NewWorkspacePage />} />
        <Route
          path=":workspace/join/:token"
          loader={loadJoinWorkspacePage}
          element={<JoinWorkspacePage />}
        />
        <Route path=":workspace" element={<WorkspaceGate />}>
          <Route index element={<DashboardPage />} />
          <Route path="settings">
            <Route index element={<GeneralSettingsPage />} />
            <Route path="members" element={<MemberSettingsPage />} />
          </Route>
        </Route>
      </Route>
    </Route>,
  ),
);
