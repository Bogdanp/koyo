import { Center, Spinner, VStack } from "@chakra-ui/react";
import * as React from "react";

export const LoadingPage = () => {
  return (
    <Center h="100vh">
      <VStack>
        <Spinner size="xl" />
      </VStack>
    </Center>
  );
};
