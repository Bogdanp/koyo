import {
  Link as ChakraLink,
  type LinkProps as ChakraLinkProps,
} from "@chakra-ui/react";
import * as React from "react";
import { NavLink as RouterNavLink } from "react-router";

export interface NavLinkProps extends ChakraLinkProps {
  end?: boolean;
  to: string;
}

export const NavLink = (props: NavLinkProps) => {
  const { children, end, to, ...root } = props;
  return (
    <ChakraLink asChild {...root}>
      <RouterNavLink {...{ end, to }}>{children}</RouterNavLink>
    </ChakraLink>
  );
};
