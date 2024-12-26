import {
  Link as ChakraLink,
  type LinkProps as ChakraLinkProps,
} from "@chakra-ui/react";
import * as React from "react";
import { Link as RouterLink } from "react-router";

export interface LinkProps extends ChakraLinkProps {
  query?: Record<string, string | string[]>;
  to: string;
}

export const Link = (props: LinkProps) => {
  const { children, query, to, ...root } = props;
  let link = to;
  if (query !== undefined) {
    const searchParams = new URLSearchParams();
    for (const param of Object.keys(query)) {
      const value = query[param];
      const values = typeof value === "string" ? [value] : value;
      for (const value of values) {
        searchParams.append(param, value);
      }
    }
    link = `${link}?${searchParams}`;
  }
  return (
    <ChakraLink asChild {...root}>
      <RouterLink to={link}>{children}</RouterLink>
    </ChakraLink>
  );
};
