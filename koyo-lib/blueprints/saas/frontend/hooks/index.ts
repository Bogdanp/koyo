import { useDispatch, useSelector } from "react-redux";

import type { AppDispatch, RootState } from "../store";

export const useAppDispatch = useDispatch.withTypes<AppDispatch>();
export const useAppSelector = useSelector.withTypes<RootState>();

export * from "./use-loading";
export * from "./use-title";
export * from "./use-toaster";
export * from "./use-workspace";
export * from "./use-workspace-link";
