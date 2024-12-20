import { Link } from "@chakra-ui/react";
import * as React from "react";
import { NavLink as RouterNavLink } from "react-router";

export interface NavLinkProps {
  to: string;
  children?: React.ReactNode;
}

export const NavLink = (props: NavLinkProps) => {
  return (
    <Link asChild color="fg">
      <RouterNavLink to={props.to}>{props.children}</RouterNavLink>
    </Link>
  );
};
