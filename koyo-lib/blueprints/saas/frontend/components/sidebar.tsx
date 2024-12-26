import { Flex, FlexProps } from "@chakra-ui/react";
import * as React from "react";

export interface SidebarProps extends FlexProps {}

export const Sidebar = (props: SidebarProps) => {
  const { children, ...root } = props;
  return (
    <Flex
      alignItems="flex-start"
      alignContent="flex-start"
      borderRightWidth="1px"
      direction="column"
      w="100%"
      {...root}
    >
      {children}
    </Flex>
  );
};
