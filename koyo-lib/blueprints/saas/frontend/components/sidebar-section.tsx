import { Stack, StackProps, Text } from "@chakra-ui/react";
import * as React from "react";

export interface SidebarSectionProps extends StackProps {
  title?: string;
}

export const SidebarSection = (props: SidebarSectionProps) => {
  const { children, title, ...root } = props;
  return (
    <Stack w="100%" gap="1px" p="2" {...root}>
      {title && (
        <Text
          color="fg.muted"
          fontSize="sm"
          pb="1"
          pl="2"
          style={{ cursor: "default" }}
        >
          {title}
        </Text>
      )}
      {children}
    </Stack>
  );
};
