import * as React from "react";
import { Outlet, useNavigate } from "react-router";

import { useAppSelector, useWorkspace } from "../hooks";
import { LoadingPage } from "../pages/loading";
import { WorkspaceStateStatus } from "../store/slices/workspace";

export const WorkspaceGate = () => {
  const { status } = useAppSelector((s) => s.workspace);
  const navigate = useNavigate();
  const workspace = useWorkspace();
  React.useEffect(() => {
    if (status !== WorkspaceStateStatus.LOADED) return;
    if (workspace === undefined) {
      navigate("/workspaces");
    }
  }, [navigate, status, workspace]);
  if (workspace !== undefined) {
    return <Outlet />;
  }
  return <LoadingPage />;
};
