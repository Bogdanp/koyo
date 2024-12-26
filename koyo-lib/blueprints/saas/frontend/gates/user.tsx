import * as React from "react";
import { Outlet, useNavigate } from "react-router";

import { useAppSelector } from "../hooks";
import { LoadingPage } from "../pages/loading";
import { AuthStateStatus } from "../store/slices/auth";

export interface UserGateProps {
  navigateTo?: string;
}

export const UserGate = (props: UserGateProps) => {
  const { navigateTo } = props;
  const { status } = useAppSelector((s) => s.auth);
  const navigate = useNavigate();
  React.useEffect(() => {
    switch (status) {
      case AuthStateStatus.LOADING:
        return;
      case AuthStateStatus.LOADED:
        if (navigateTo !== undefined) {
          navigate(navigateTo);
        }
        return;
      case AuthStateStatus.FAILED:
        navigate("/login");
        return;
    }
  }, [navigate, navigateTo, status]);
  if (status === AuthStateStatus.LOADED) {
    return <Outlet />;
  }
  return <LoadingPage />;
};
