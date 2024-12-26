import {
  Flex,
  HStack,
  IconButton,
  Spacer,
  Stack,
  StackProps,
} from "@chakra-ui/react";
import * as React from "react";
import { LuHouse, LuMenu, LuSettings } from "react-icons/lu";

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
import { WorkspaceMenu } from "../components/workspace-menu";
import { useWorkspaceLink } from "../hooks";

export interface WorkspaceLayoutProps extends StackProps {}

export const WorkspaceLayout = (props: WorkspaceLayoutProps) => {
  const { children, ...root } = props;
  const link = useWorkspaceLink();
  const sections = (
    <>
      <SidebarSection>
        <NavItem to={link("/")} icon={<LuHouse />}>
          Dashboard
        </NavItem>
      </SidebarSection>
      <Spacer />
      <SidebarSection>
        <NavItem to={link("/settings")} icon={<LuSettings />}>
          Settings
        </NavItem>
      </SidebarSection>
    </>
  );
  return (
    <>
      <Navbar hideFrom="md">
        <HStack flex="1">
          <WorkspaceMenu />
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
          <HStack p="2">
            <WorkspaceMenu />
          </HStack>
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
