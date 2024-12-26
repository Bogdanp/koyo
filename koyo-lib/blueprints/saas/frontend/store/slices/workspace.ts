import { RootState } from "..";

import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";

import { Workspace, getWorkspaces } from "../../api/workspaces";

export enum WorkspaceStateStatus {
  LOADING,
  LOADED,
}

export interface WorkspaceState {
  status: WorkspaceStateStatus;
  workspaces: Workspace[];
}

export const loadWorkspaces = createAsyncThunk<
  Workspace[],
  void,
  { state: RootState }
>("workspace/loadWorkspaces", async () => {
  return await getWorkspaces();
});

export const workspaceSlice = createSlice({
  name: "workspace",
  initialState: {
    status: WorkspaceStateStatus.LOADING,
    workspaces: [],
  } as WorkspaceState,
  reducers: {},
  extraReducers: (builder) => {
    builder.addCase(loadWorkspaces.pending, (state) => {
      state.status = WorkspaceStateStatus.LOADING;
    });
    builder.addCase(loadWorkspaces.fulfilled, (state, action) => {
      state.status = WorkspaceStateStatus.LOADED;
      state.workspaces = action.payload;
    });
    builder.addCase(loadWorkspaces.rejected, (state) => {
      state.status = WorkspaceStateStatus.LOADED;
      state.workspaces = [];
    });
  },
});

export default workspaceSlice.reducer;
