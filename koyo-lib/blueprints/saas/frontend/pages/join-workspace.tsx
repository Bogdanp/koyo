import { VStack } from "@chakra-ui/react";
import * as React from "react";
import {
  LoaderFunctionArgs,
  useLoaderData,
  useLocation,
  useNavigate,
} from "react-router";

import {
  WorkspaceInvite,
  getWorkspaceInvite,
  joinWorkspace,
} from "../api/workspaces";
import { Button } from "../components/chakra/button";
import { Link } from "../components/link";
import { useAppSelector, useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";

interface Data {
  invite: WorkspaceInvite;
  token: string;
  workspaceId: number;
}

export const loadJoinWorkspacePage = async ({
  params,
}: LoaderFunctionArgs): Promise<Data> => {
  const { workspace, token } = params;
  const workspaceId = Number(workspace);
  const invite = await getWorkspaceInvite(workspaceId, token!);
  return { invite, token: token!, workspaceId };
};

export const JoinWorkspacePage = () => {
  useTitle("Join Workspace");
  const { invite, token, workspaceId }: Data = useLoaderData();
  const { user } = useAppSelector((s) => s.auth);
  const location = useLocation();
  const navigate = useNavigate();
  const withToast = useToaster();
  const [isLoading, withLoading] = useLoading();
  const onJoin = React.useCallback(async () => {
    await withLoading(async () => {
      await withToast(async () => {
        await joinWorkspace(workspaceId, token);
        navigate("/workspaces");
      });
    });
  }, [navigate, token, withLoading, withToast, workspaceId]);
  return (
    <AuthLayout
      title="Join Workspace"
      subtitle={`Join "${invite.workspace}" as ${user!.username}?`}
    >
      <VStack>
        <Button loading={isLoading} size="xl" onClick={onJoin}>
          Join Workspace
        </Button>
        <Link
          color="fg.muted"
          fontSize="sm"
          query={{ return: location.pathname }}
          to="/login"
        >
          Log in to a different account.
        </Link>
      </VStack>
    </AuthLayout>
  );
};
