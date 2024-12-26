import { HStack, Image, Text } from "@chakra-ui/react";
import * as React from "react";

export interface LogoProps {
  size?: "xs" | "sm" | "md" | "lg";
}

export const Logo = (props: LogoProps) => {
  let { size } = props;
  let width = "48px";
  switch (size) {
    case "xs":
      width = "32px";
      break;
    case "sm":
      width = "48px";
      break;
    case "md":
      width = "96px";
      break;
    case "lg":
      width = "192px";
      break;
  }
  size = size ?? "md";
  return (
    <HStack>
      <Image src="/static/img/koyo.png" width={width} height={width} />
      <Text color="fg" fontSize="xl" fontWeight="semibold">
        SaasExample
      </Text>
    </HStack>
  );
};
