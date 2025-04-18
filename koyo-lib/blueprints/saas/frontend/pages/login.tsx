import { HStack, Input, Stack, Text } from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { useLocation, useNavigate } from "react-router";
import { z } from "zod";

import { Button } from "../components/chakra/button";
import { Checkbox } from "../components/chakra/checkbox";
import { Field } from "../components/chakra/field";
import { PasswordInput } from "../components/chakra/password-input";
import { Link } from "../components/link";
import { useAppDispatch, useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";
import { login } from "../store/slices/auth";

const formSchema = z.object({
  username: z.string().email(),
  password: z.string().min(8),
});

type FormData = z.infer<typeof formSchema>;

export const LoginPage = () => {
  useTitle("Login");
  const {
    handleSubmit,
    formState: { errors },
    register,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const withToaster = useToaster();
  const [isLoading, withLoading] = useLoading();
  const dispatch = useAppDispatch();
  const navigate = useNavigate();
  const location = useLocation();
  const params = React.useMemo(
    () => new URLSearchParams(location.search),
    [location],
  );
  const onSubmit = handleSubmit((data) => {
    withLoading(async () => {
      await withToaster(async () => {
        await dispatch(login(data)).unwrap();
        navigate(params.get("return") || "/workspaces");
      });
    });
  });

  return (
    <AuthLayout
      title="Welcome Back"
      subtitle="Start using AppNameHere in your projects."
    >
      <form onSubmit={onSubmit}>
        <Stack gap="6">
          <Stack gap="5">
            <Field
              label="Email"
              invalid={!!errors.username}
              errorText={errors.username?.message}
            >
              <Input type="email" {...register("username")} />
            </Field>
            <Field
              label="Password"
              invalid={!!errors.password}
              errorText={errors.password?.message}
            >
              <PasswordInput {...register("password")} />
            </Field>
          </Stack>
          <HStack justify="space-between">
            <Checkbox defaultChecked>Remember me</Checkbox>
            <Link to="/reset-password">Reset Password</Link>
          </HStack>
          <Stack gap="4">
            <Button loading={isLoading} type="submit">
              Sign in
            </Button>
          </Stack>
        </Stack>
      </form>

      <Text textStyle="sm" color="fg.muted" textAlign="center">
        Don{"'"}t have an account?{" "}
        <Link variant="underline" to="/sign-up">
          Sign up
        </Link>
      </Text>
    </AuthLayout>
  );
};
