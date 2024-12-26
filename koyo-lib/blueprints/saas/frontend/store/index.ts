import { configureStore } from "@reduxjs/toolkit";

import authReducer, * as auth from "./slices/auth";
import workspaceReducer from "./slices/workspace";

export const store = configureStore({
  reducer: {
    auth: authReducer,
    workspace: workspaceReducer,
  },
});

export type RootState = ReturnType<typeof store.getState>;
export type AppDispatch = typeof store.dispatch;

export const initStore = () => {
  store.dispatch(auth.loadUser());
};
