import { Input, Stack, Text } from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { useNavigate } from "react-router";
import { z } from "zod";

import { createWorkspace } from "../api/workspaces";
import { Button } from "../components/chakra/button";
import { Field } from "../components/chakra/field";
import { Link } from "../components/link";
import { useAppDispatch, useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";
import { loadWorkspaces } from "../store/slices/workspace";

const formSchema = z.object({
  name: z.string().min(6),
  slug: z.string().min(6),
});

type FormData = z.infer<typeof formSchema>;

export const NewWorkspacePage = () => {
  useTitle("Create a Workspace");

  const {
    handleSubmit,
    formState: { errors },
    register,
    setValue,
    watch,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const withToaster = useToaster();
  const [isLoading, withLoading] = useLoading();
  const navigate = useNavigate();
  const dispatch = useAppDispatch();
  const onSubmit = handleSubmit((data) => {
    withLoading(async () => {
      await withToaster(async () => {
        await createWorkspace(data);
        await dispatch(loadWorkspaces()).unwrap();
        navigate("/");
      });
    });
  });
  const name = watch("name", "");
  React.useEffect(() => {
    setValue("slug", slugify(name));
  }, [name, setValue]);

  return (
    <AuthLayout title="Create a Workspace">
      <form onSubmit={onSubmit}>
        <Stack gap="6">
          <Stack gap="5">
            <Field
              label="Workspace Name"
              invalid={!!errors.name}
              errorText={errors.name?.message}
            >
              <Input {...register("name")} />
            </Field>
            <Field
              label="Workspace Slug"
              invalid={!!errors.slug}
              errorText={errors.slug?.message}
            >
              <Input disabled {...register("slug")} />
            </Field>
          </Stack>
          <Stack gap="4">
            <Button loading={isLoading} type="submit">
              Create Workspace
            </Button>
          </Stack>
        </Stack>
      </form>

      <Text textStyle="sm" color="fg.muted" textAlign="center">
        <Link variant="underline" to="/workspaces">
          Open an Existing Workspace
        </Link>
      </Text>
    </AuthLayout>
  );
};

const slugify = (name: string): string => {
  return name
    .toLowerCase()
    .replace(/[^a-z0-9]/g, "-")
    .replace(/-+/g, "-");
};
