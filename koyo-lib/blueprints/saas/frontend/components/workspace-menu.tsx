import { Bleed, Separator, Text } from "@chakra-ui/react";
import * as React from "react";
import { useNavigate } from "react-router";

import { useAppSelector } from "../hooks";
import { MenuContent, MenuItem, MenuRoot, MenuTrigger } from "./chakra/menu";
import { Link } from "./link";
import { Logo } from "./logo";

export const WorkspaceMenu = () => {
  const { workspaces } = useAppSelector((s) => s.workspace);
  const navigate = useNavigate();
  const onWorkspaceSelect = React.useCallback(
    (slug: string) => {
      const workspace = workspaces.find((w) => w.slug === slug);
      if (workspace) {
        navigate(`/workspaces/${slug}`);
      }
    },
    [navigate, workspaces],
  );

  return (
    <MenuRoot onSelect={(details) => onWorkspaceSelect(details.value)}>
      <MenuTrigger
        _hover={{ background: "bg.muted", cursor: "pointer" }}
        borderRadius="xl"
        p="1"
      >
        <Logo />
      </MenuTrigger>
      <MenuContent w="2xs">
        <Text fontSize="sm" fontWeight="bold" p="2">
          Workspaces
        </Text>
        {workspaces.map((w) => (
          <MenuItem key={w.id} value={w.slug}>
            {w.name}
          </MenuItem>
        ))}
        <Bleed inline="2">
          <Separator mt="2" mb="2" />
        </Bleed>
        <MenuItem asChild value="$create">
          <Link _hover={{ textDecoration: "none" }} to="/workspaces/new">
            Create a Workspace
          </Link>
        </MenuItem>
      </MenuContent>
    </MenuRoot>
  );
};
