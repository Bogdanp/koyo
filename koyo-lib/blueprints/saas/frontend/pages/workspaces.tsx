import {
  Box,
  Center,
  LinkBox,
  LinkOverlay,
  Spinner,
  Stack,
  Text,
} from "@chakra-ui/react";
import * as React from "react";
import { useNavigate } from "react-router";

import { Link } from "../components/link";
import { useAppSelector, useTitle } from "../hooks";
import { AuthLayout } from "../layouts/auth";
import { WorkspaceStateStatus } from "../store/slices/workspace";

export const WorkspacesPage = () => {
  useTitle("Workspaces");

  const { status, workspaces } = useAppSelector((s) => s.workspace);
  const navigate = useNavigate();
  React.useEffect(() => {
    if (status !== WorkspaceStateStatus.LOADED) return;
    if (workspaces.length === 0) {
      navigate("new");
    }
  }, [navigate, status, workspaces]);

  return (
    <AuthLayout
      title="Workspaces"
      subtitle={
        <Text color="fg.muted">
          Open a workspace or{" "}
          <Link variant="underline" to="new">
            create one
          </Link>
          .
        </Text>
      }
    >
      <Box borderRadius="md" borderWidth="1px">
        {status !== WorkspaceStateStatus.LOADED ? (
          <Center p="4">
            <Spinner />
          </Center>
        ) : (
          workspaces.map((w, idx) => (
            <LinkBox key={w.id}>
              <Stack
                borderBottomWidth={idx < workspaces.length - 1 ? "1px" : "0px"}
                gap="0"
                p="4"
              >
                <Text fontWeight="semibold">{w.name}</Text>
                <LinkOverlay asChild>
                  <Link
                    color="fg.muted"
                    fontSize="xs"
                    to={`/workspaces/${w.slug}`}
                  >
                    {w.slug}
                  </Link>
                </LinkOverlay>
              </Stack>
            </LinkBox>
          ))
        )}
      </Box>
    </AuthLayout>
  );
};
