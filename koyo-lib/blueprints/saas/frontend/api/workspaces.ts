import axios from "axios";

export interface CreateWorkspaceData {
  name: string;
  slug: string;
}

export const createWorkspace = async (
  data: CreateWorkspaceData,
): Promise<Workspace> => {
  return (await axios.post("/api/v1/workspaces", data)).data;
};

export interface Workspace {
  id: number;
  name: string;
  slug: string;
  "created-at": string;
  "updated-at": string;
}

export const getWorkspaces = async (): Promise<Workspace[]> => {
  return (await axios.get("/api/v1/workspaces")).data;
};

export enum WorkspaceMemberRole {
  ADMIN = "admin",
  MEMBER = "member",
}

export enum WorkspaceMemberStatus {
  PENDING = "pending",
  ACTIVE = "active",
}

export interface WorkspaceMember {
  id: number;
  email: string;
  role: WorkspaceMemberRole;
  status: WorkspaceMemberStatus;
}

export const getWorkspaceMembers = async (
  workspace: Workspace,
): Promise<WorkspaceMember[]> => {
  return (await axios.get(`/api/v1/workspaces/${workspace.id}/members`)).data;
};

export const deleteWorkspaceMember = async (
  workspace: Workspace,
  memberId: number,
): Promise<void> => {
  return axios.delete(`/api/v1/workspaces/${workspace.id}/members/${memberId}`);
};

export interface InviteWorkspaceMemberData {
  email: string;
}

export const inviteWorkspaceMember = async (
  workspace: Workspace,
  data: InviteWorkspaceMemberData,
): Promise<void> => {
  await axios.post(`/api/v1/workspaces/${workspace.id}/invites`, data);
};

export interface WorkspaceInvite {
  workspace: string;
}

export const getWorkspaceInvite = async (
  workspaceId: number,
  inviteToken: string,
): Promise<WorkspaceInvite> => {
  return (
    await axios.get(`/api/v1/workspaces/${workspaceId}/invites/${inviteToken}`)
  ).data;
};

export const joinWorkspace = async (
  workspaceId: number,
  inviteToken: string,
): Promise<void> => {
  await axios.post(
    `/api/v1/workspaces/${workspaceId}/invites/${inviteToken}/join`,
  );
};

export const deleteWorkspaceMemberInvite = async (
  workspace: Workspace,
  inviteId: number,
): Promise<void> => {
  await axios.delete(`/api/v1/workspaces/${workspace.id}/invites/${inviteId}`);
};
