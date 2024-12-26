import {
  Flex,
  HStack,
  IconButton,
  Spacer,
  Stack,
  StackProps,
  Text,
} from "@chakra-ui/react";
import * as React from "react";
import { LuArrowLeft, LuMenu, LuSettings, LuUsers } from "react-icons/lu";

import {
  PopoverContent,
  PopoverRoot,
  PopoverTrigger,
} from "../components/chakra/popover";
import { Toaster } from "../components/chakra/toaster";
import { NavItem } from "../components/nav-item";
import { Navbar } from "../components/navbar";
import { Sidebar } from "../components/sidebar";
import { SidebarSection } from "../components/sidebar-section";
import { useWorkspaceLink } from "../hooks";

export interface SettingsLayoutProps extends StackProps {}

export const SettingsLayout = (props: SettingsLayoutProps) => {
  const { children, ...root } = props;
  const link = useWorkspaceLink();
  const sections = (
    <>
      <SidebarSection title="Workspace">
        <NavItem end to={link("/settings")} icon={<LuSettings />}>
          General
        </NavItem>
        <NavItem to={link("/settings/members")} icon={<LuUsers />}>
          Members
        </NavItem>
      </SidebarSection>
    </>
  );
  return (
    <>
      <Navbar hideFrom="md">
        <HStack flex="1">
          <BackButton />
          <Spacer />
          <PopoverRoot>
            <PopoverTrigger asChild>
              <IconButton color="fg" size="md" variant="ghost">
                <LuMenu />
              </IconButton>
            </PopoverTrigger>
            <PopoverContent>{sections}</PopoverContent>
          </PopoverRoot>
        </HStack>
      </Navbar>
      <Flex flex="1">
        <Sidebar
          height="100vh"
          hideBelow="md"
          maxW="2xs"
          position="sticky"
          top="0"
        >
          <BackButton />
          {sections}
        </Sidebar>
        <Stack gap="12" pb="12" flex="1" alignItems="stretch" {...root}>
          {children}
        </Stack>
      </Flex>
      <Toaster />
    </>
  );
};

const BackButton = () => {
  const link = useWorkspaceLink();
  return (
    <HStack fontSize="xs" gap="1" p="2">
      <NavItem color="fg" end fontSize="md" to={link("")}>
        <LuArrowLeft />
      </NavItem>
      <Text fontSize="lg">Settings</Text>
    </HStack>
  );
};
