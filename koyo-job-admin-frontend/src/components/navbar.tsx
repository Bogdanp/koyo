import { Flex, FlexProps } from "@chakra-ui/react";
import * as React from "react";

export interface NavbarProps extends FlexProps {
  children?: React.ReactNode;
}

export const Navbar = (props: NavbarProps) => {
  return <Flex borderBottomWidth="1px" minH="16" pl="8" gap="4" {...props} />;
};
