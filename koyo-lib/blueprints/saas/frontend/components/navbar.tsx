import { Flex, FlexProps } from "@chakra-ui/react/flex";
import * as React from "react";

export interface NavbarProps extends FlexProps {}

export const Navbar = (props: NavbarProps) => {
  const { children, ...root } = props;
  return (
    <Flex borderBottomWidth="1px" w="100%" {...root}>
      {children}
    </Flex>
  );
};
