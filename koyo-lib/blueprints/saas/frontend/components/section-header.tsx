import { Heading, Stack, StackProps, Text } from "@chakra-ui/react";
import * as React from "react";

export interface SectionHeaderProps extends StackProps {
  title: string;
  subtitle?: string;
}

export const SectionHeader = (props: SectionHeaderProps) => {
  const { title, subtitle, ...root } = props;
  return (
    <Stack gap="0" {...root}>
      <Heading fontSize="2xl">{title}</Heading>
      {subtitle && (
        <Text color="fg.muted" fontSize="md">
          {subtitle}
        </Text>
      )}
    </Stack>
  );
};
