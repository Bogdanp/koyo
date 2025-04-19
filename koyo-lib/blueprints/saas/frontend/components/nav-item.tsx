import { HStack, LinkBox, LinkOverlay } from "@chakra-ui/react";
import * as React from "react";
import { useLocation } from "react-router";

import { NavLink, type NavLinkProps } from "./nav-link";

export interface NavItemProps extends NavLinkProps {
  icon?: React.ReactNode;
}

export const NavItem = (props: NavItemProps) => {
  const { children, icon, ...root } = props;
  const location = useLocation();
  const isActive = React.useMemo(() => {
    const trimmedTo = props.to.replace(/\/$/, "");
    const trimmedPath = location.pathname.replace(/\/$/, "");
    return (
      (props.end && trimmedPath === trimmedTo) ||
      (!props.end && trimmedPath.startsWith(trimmedTo))
    );
  }, [location, props]);
  return (
    <LinkBox
      _hover={{ background: "bg.muted" }}
      background={isActive ? "bg.muted" : "bg"}
      borderRadius="md"
      fontSize="sm"
      flex="1"
      pl="2"
      pr="2"
      pt="1"
      pb="1"
      style={{ transition: "background 0.10s" }}
    >
      <HStack>
        {icon}
        <LinkOverlay asChild>
          <NavLink
            _hover={{ textDecoration: "none" }}
            fontSize="sm"
            color="fg"
            {...root}
          >
            {children}
          </NavLink>
        </LinkOverlay>
      </HStack>
    </LinkBox>
  );
};
