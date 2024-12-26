import { Container, Heading, Stack, Text } from "@chakra-ui/react";
import * as React from "react";
import { useRouteError } from "react-router";

export const ErrorPage = () => {
  const error: any = useRouteError();
  const message = React.useMemo(() => {
    switch (error.status) {
      case 404:
        return "Page not found.";
      default:
        return error.data;
    }
  }, [error]);
  return (
    <Container pt="4">
      <Stack>
        <Heading fontSize="xl">{error.statusText}</Heading>
        <Text>{message}</Text>
      </Stack>
    </Container>
  );
};
