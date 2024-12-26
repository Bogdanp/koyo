import {
  Center,
  Container,
  type ContainerProps,
  Heading,
  Stack,
  Text,
} from "@chakra-ui/react";
import * as React from "react";

import { Toaster } from "../components/chakra/toaster";
import { Link } from "../components/link";
import { Logo } from "../components/logo";

export interface AuthLayoutProps extends ContainerProps {
  title: string;
  subtitle?: React.ReactNode;
}

export const AuthLayout = (props: AuthLayoutProps) => {
  const { children, title, subtitle, ...root } = props;
  let subtitleNode: React.ReactNode = subtitle;
  if (typeof subtitle === "string") {
    subtitleNode = <Text color="fg.muted">{subtitle}</Text>;
  }

  return (
    <Container maxW="md" py={{ base: "12", md: "24" }} {...root}>
      <Stack gap="8">
        <Center>
          <Link to="/workspaces">
            <Logo size="xs" />
          </Link>
        </Center>
        <Stack gap={{ base: "2", md: "3" }} textAlign="center">
          <Heading size={{ base: "2xl", md: "3xl" }}>{title}</Heading>
          {subtitleNode}
        </Stack>
        {children}
        <Toaster />
      </Stack>
    </Container>
  );
};
