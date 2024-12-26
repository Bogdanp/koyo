import { Input, Stack, Text } from "@chakra-ui/react";
import { zodResolver } from "@hookform/resolvers/zod";
import * as React from "react";
import { useForm } from "react-hook-form";
import { useNavigate } from "react-router";
import { z } from "zod";

import { createUser } from "../api/users";
import { Button } from "../components/chakra/button";
import { Field } from "../components/chakra/field";
import { PasswordInput } from "../components/chakra/password-input";
import { Link } from "../components/link";
import { useLoading, useTitle, useToaster } from "../hooks";
import { AuthLayout } from "../layouts/auth";

const formSchema = z.object({
  username: z.string().email(),
  password: z.string().min(8),
});

type FormData = z.infer<typeof formSchema>;

export const SignUpPage = () => {
  useTitle("Sign Up");

  const {
    handleSubmit,
    formState: { errors },
    register,
  } = useForm<FormData>({
    resolver: zodResolver(formSchema),
  });
  const withToaster = useToaster();
  const [isLoading, withLoading] = useLoading();
  const navigate = useNavigate();
  const onSubmit = handleSubmit((data) => {
    withLoading(async () => {
      await withToaster(async () => {
        await createUser(data);
        navigate("/login");
      });
    });
  });

  return (
    <AuthLayout title="Sign Up" subtitle="Create an Account.">
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
          <Button loading={isLoading} type="submit">
            Sign up
          </Button>
        </Stack>
      </form>

      <Text textStyle="sm" color="fg.muted" textAlign="center">
        Already have an account?{" "}
        <Link variant="underline" to="/login">
          Sign in
        </Link>
      </Text>
    </AuthLayout>
  );
};
