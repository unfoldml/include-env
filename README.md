# include-env

Embed secrets (e.g. API keys) from environment variables inside production artifacts without checking them into the repository. 

This use case is relevant e.g. in continuous integration (CI) settings.

# Example 

In GitHub each private repository can have a number of secrets, which can be conveniently changed without being exposed as needed; GitHub Actions can pass repository secrets in as environment variables during any step, which is where this library comes in handy.
