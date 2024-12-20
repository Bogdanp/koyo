import { Link as ChakraLink } from "@chakra-ui/react";
import * as React from "react";
import { Link as RouterLink } from "react-router";

export interface LinkProps {
  to: string;
  children?: React.ReactNode;
}

export const Link = (props: LinkProps) => {
  return (
    <ChakraLink asChild color="fg">
      <RouterLink to={props.to}>{props.children}</RouterLink>
    </ChakraLink>
  );
};
