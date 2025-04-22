import {
  Badge,
  Container,
  IconButton,
  Input,
  Spinner,
  Stack,
  Table,
  createListCollection,
} from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { Controller, useForm } from "react-hook-form";
import { LuEllipsis, LuTrash, LuUserPlus } from "react-icons/lu";
import { z } from "zod";

import {
  Workspace,
  WorkspaceMember,
  WorkspaceMemberRole,
  WorkspaceMemberStatus,
  deleteWorkspaceMember,
  deleteWorkspaceMemberInvite,
  getWorkspaceMembers,
  inviteWorkspaceMember,
} from "../api";
import { Avatar } from "../components/chakra/avatar";
import { Button } from "../components/chakra/button";
import {
  DialogActionTrigger,
  DialogBackdrop,
  DialogBody,
  DialogCloseTrigger,
  DialogContent,
  DialogFooter,
  DialogHeader,
  DialogRoot,
  DialogTitle,
  DialogTrigger,
} from "../components/chakra/dialog";
import { Field } from "../components/chakra/field";
import {
  MenuContent,
  MenuItem,
  MenuRoot,
  MenuTrigger,
} from "../components/chakra/menu";
import {
  SelectContent,
  SelectItem,
  SelectRoot,
  SelectTrigger,
  SelectValueText,
} from "../components/chakra/select";
import { SectionHeader } from "../components/section-header";
import { useAppSelector, useLoading, useToaster, useWorkspace } from "../hooks";
import { SettingsLayout } from "../layouts/settings";

const formSchema = z.object({
  email: z.string().email(),
  role: z.nativeEnum(WorkspaceMemberRole),
});

type FormData = z.infer<typeof formSchema>;

export const MemberSettingsPage = () => {
  const workspace = useWorkspace()!;
  const [members, setMembers] = React.useState<WorkspaceMember[]>([]);
  const [loading, withLoading] = useLoading();
  const loadMembers = React.useCallback(async () => {
    await withLoading(async () => {
      setMembers(await getWorkspaceMembers(workspace));
    });
  }, [withLoading, workspace]);
  React.useEffect(() => {
    loadMembers();
  }, [loadMembers]);

  const withToast = useToaster();
  const [inviteDialogOpen, setInviteDialogOpen] = React.useState(false);
  const [inviteDialogLoading, withInviteDialogLoading] = useLoading();
  const inviteMember = React.useCallback(
    async (data: FormData) => {
      await withInviteDialogLoading(async () => {
        await withToast(async () => {
          await inviteWorkspaceMember(workspace, data);
          await loadMembers();
          setInviteDialogOpen(false);
        });
      });
    },
    [loadMembers, withInviteDialogLoading, withToast, workspace],
  );

  return (
    <SettingsLayout>
      <Container pt="8" maxW="4xl">
        <Stack>
          <SectionHeader
            title="Workspace Members"
            subtitle="Manage who has access to your workspace."
          />

          <Table.Root borderRadius="sm" maxW="xl" p="2" variant="outline">
            <Table.Body>
              {loading ? (
                <Table.Row>
                  <Table.Cell>
                    <Spinner m="2" size="xl" />
                  </Table.Cell>
                </Table.Row>
              ) : (
                <>
                  <Table.Row>
                    <Table.Cell colSpan={5} textAlign="end">
                      <DialogRoot
                        size="sm"
                        open={inviteDialogOpen}
                        onOpenChange={(e) => setInviteDialogOpen(e.open)}
                      >
                        <DialogBackdrop />
                        <DialogTrigger asChild>
                          <Button size="xs">
                            <LuUserPlus /> Invite Member
                          </Button>
                        </DialogTrigger>
                        <InviteMemberDialogContent
                          loading={inviteDialogLoading}
                          onInviteMember={inviteMember}
                        />
                      </DialogRoot>
                    </Table.Cell>
                  </Table.Row>

                  {members.map((m) => (
                    <MemberRow
                      key={m.email}
                      member={m}
                      workspace={workspace}
                      onChange={() => loadMembers()}
                    />
                  ))}
                </>
              )}
            </Table.Body>
          </Table.Root>
        </Stack>
      </Container>
    </SettingsLayout>
  );
};

interface MemberRowProps {
  member: WorkspaceMember;
  workspace: Workspace;
  onChange?: () => void;
}

enum MemberAction {
  DELETE = "delete",
}

const MemberRow = (props: MemberRowProps) => {
  const { user } = useAppSelector((s) => s.auth);
  const { member, workspace, onChange } = props;
  const withToast = useToaster();
  const onSelect = React.useCallback(
    async (action: MemberAction) => {
      switch (action) {
        case MemberAction.DELETE:
          if (confirm("Delete this member?")) {
            await withToast(async () => {
              switch (member.status) {
                case WorkspaceMemberStatus.ACTIVE:
                  await deleteWorkspaceMember(workspace, member.id);
                  return;
                case WorkspaceMemberStatus.PENDING:
                  await deleteWorkspaceMemberInvite(workspace, member.id);
                  return;
              }
            });
            if (onChange !== undefined) {
              onChange();
            }
          }
          return;
      }
    },
    [member, onChange, withToast, workspace],
  );

  return (
    <Table.Row key={member.email}>
      <Table.Cell>
        <Avatar name={member.email} />
      </Table.Cell>
      <Table.Cell>{member.email}</Table.Cell>
      <Table.Cell>
        <Badge>{member.role}</Badge>
      </Table.Cell>
      <Table.Cell>
        <Badge>{member.status}</Badge>
      </Table.Cell>
      <Table.Cell>
        <MenuRoot onSelect={(data) => onSelect(data.value as MemberAction)}>
          <MenuTrigger asChild>
            <IconButton size="sm" borderRadius="full" variant="ghost">
              <LuEllipsis />
            </IconButton>
          </MenuTrigger>
          <MenuContent>
            <MenuItem
              disabled={
                member.id === user!.id &&
                member.status === WorkspaceMemberStatus.ACTIVE
              }
              value={MemberAction.DELETE}
            >
              <LuTrash />
              Delete
            </MenuItem>
          </MenuContent>
        </MenuRoot>
      </Table.Cell>
    </Table.Row>
  );
};

const ROLES = createListCollection({
  items: [
    { label: "Member", value: "member" },
    { label: "Admin", value: "admin" },
  ],
});

interface InviteMemberDialogContentProps {
  loading: boolean;
  onInviteMember: (data: FormData) => void;
}

const InviteMemberDialogContent = (props: InviteMemberDialogContentProps) => {
  const ref = React.useRef<HTMLDivElement | null>(null);
  const {
    control,
    formState: { errors },
    handleSubmit,
    register,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const onInviteMember = handleSubmit(props.onInviteMember);
  return (
    <DialogContent ref={ref}>
      <form onSubmit={onInviteMember}>
        <DialogCloseTrigger />
        <DialogHeader>
          <DialogTitle>Invite Member</DialogTitle>
        </DialogHeader>
        <DialogBody>
          <Stack>
            <Field
              label="Email Address"
              invalid={!!errors.email}
              errorText={errors.email?.message}
            >
              <Input type="email" {...register("email")} />
            </Field>
            <Field
              label="Role"
              invalid={!!errors.role}
              errorText={errors.role?.message}
            >
              <Controller
                name="role"
                control={control}
                render={({ field }) => (
                  <SelectRoot
                    collection={ROLES}
                    name={field.name}
                    value={[field.value]}
                    onInteractOutside={() => field.onBlur()}
                    onValueChange={({ value }) => field.onChange(value[0])}
                  >
                    <SelectTrigger>
                      <SelectValueText placeholder="Select Role" />
                    </SelectTrigger>
                    <SelectContent
                      portalRef={ref as React.RefObject<HTMLElement>}
                    >
                      {ROLES.items.map((r) => (
                        <SelectItem item={r} key={r.value}>
                          {r.label}
                        </SelectItem>
                      ))}
                    </SelectContent>
                  </SelectRoot>
                )}
              />
            </Field>
          </Stack>
        </DialogBody>
        <DialogFooter>
          <DialogActionTrigger asChild>
            <Button variant="outline">Cancel</Button>
          </DialogActionTrigger>
          <Button loading={props.loading} type="submit">
            Send Invite
          </Button>
        </DialogFooter>
      </form>
    </DialogContent>
  );
};
