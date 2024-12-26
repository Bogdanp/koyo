import { RootState } from "..";

import { createAsyncThunk, createSlice } from "@reduxjs/toolkit";

import { LoginData, User, getMe } from "../../api";
import * as sessions from "../../api/sessions";
import { loadWorkspaces } from "./workspace";

export enum AuthStateStatus {
  LOADING,
  LOADED,
  FAILED,
}

export interface AuthState {
  status: AuthStateStatus;
  user: User | null;
}

export const login = createAsyncThunk<void, LoginData, { state: RootState }>(
  "auth/login",
  async (data, { dispatch, rejectWithValue }) => {
    try {
      await sessions.login(data);
    } catch (error) {
      return rejectWithValue(error);
    }
    dispatch(loadUser());
  },
);

export const loadUser = createAsyncThunk<User, void, { state: RootState }>(
  "auth/loadUser",
  async (_, { dispatch }) => {
    const user = await getMe();
    dispatch(loadWorkspaces());
    return user;
  },
);

export const authSlice = createSlice({
  name: "auth",
  initialState: {
    status: AuthStateStatus.LOADING,
    user: null,
  } as AuthState,
  reducers: {},
  extraReducers: (builder) => {
    builder.addCase(loadUser.pending, (state) => {
      state.status = AuthStateStatus.LOADING;
    });
    builder.addCase(loadUser.fulfilled, (state, action) => {
      state.status = AuthStateStatus.LOADED;
      state.user = action.payload;
    });
    builder.addCase(loadUser.rejected, (state) => {
      state.status = AuthStateStatus.FAILED;
      state.user = null;
    });
  },
});

export default authSlice.reducer;
