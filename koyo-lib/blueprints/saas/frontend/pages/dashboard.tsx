import { Container } from "@chakra-ui/react";
import * as React from "react";

import { SectionHeader } from "../components/section-header";
import { useTitle, useWorkspace } from "../hooks";
import { WorkspaceLayout } from "../layouts/workspace";

export const DashboardPage = () => {
  const workspace = useWorkspace()!;
  useTitle(workspace.name);
  return (
    <WorkspaceLayout>
      <Container maxW="4xl" pt="8">
        <SectionHeader
          title="Dashboard"
          subtitle="Exciting app content goes here..."
        />
      </Container>
    </WorkspaceLayout>
  );
};
