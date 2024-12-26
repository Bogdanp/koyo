import { useAppSelector } from ".";

import { useParams } from "react-router";

import { Workspace } from "../api";

export const useWorkspace = (): Workspace | undefined => {
  const { workspace } = useParams();
  const { workspaces } = useAppSelector((s) => s.workspace);
  return workspaces.find((w) => w.slug === workspace);
};
