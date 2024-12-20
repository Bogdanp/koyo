import { Stack } from "@chakra-ui/react";
import * as React from "react";
import { LuHouse } from "react-icons/lu";

import { NavLink } from "./nav-link";
import { Navbar } from "./navbar";

export interface LayoutProps {
  children?: React.ReactNode;
}

export const Layout = (props: LayoutProps) => {
  return (
    <>
      <Navbar>
        <NavLink to="/">
          <LuHouse /> Dashboard
        </NavLink>
      </Navbar>
      <Stack gap="12" pb="12" flex="1" alignItems="stretch">
        {props.children}
      </Stack>
    </>
  );
};
