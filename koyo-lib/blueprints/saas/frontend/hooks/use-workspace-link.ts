import { useWorkspace } from ".";

import * as React from "react";

export const useWorkspaceLink = () => {
  const workspace = useWorkspace()!;
  const workspaceLink = React.useCallback(
    (link: string) => {
      return `/workspaces/${workspace.slug}${link}`;
    },
    [workspace],
  );
  return workspaceLink;
};
