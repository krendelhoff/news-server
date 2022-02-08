# The News server

## Description
The News REST API server. 

## Personal goals
- Asynchronous logger
- Servant-like type-level routing implementation
- Using TH in favor of Type Driven Development
- Effect tracking system
- As a result - testable code and extensibility

## Setup
Clone current repository:
```
git clone git@github.com:krendelhoff/news-server.git
cd news-server
```

Install postgres and create database:
```
initdb db_name
pg_ctl -D db_name start
createdb db_name
```

Enter psql utility as superuser and create two users: one admin for database changes and second user for general server usage:

```
psql -d database_name

CREATE USER admin_name PASSWORD 'admin_pass';
CREATE USER user_name PASSWORD 'user_pass';
```

Next you need to assign priveleges to created users:

```
GRANT ALL PRIVILEGES ON DATABASE db_name TO admin_name;
GRANT CONNECT ON  DATABASE db_name TO user_name;
GRANT USAGE ON SCHEMA public TO user_name;
GRANT USAGE ON ALL SEQUENCES IN SCHEMA public TO user_name;
GRANT SELECT, INSERT, DELETE, UPDATE ON ALL TABLES IN SCHEMA public TO user_name;
```
Done! You can leave psql with `\q`.

To run server, you have to provide configuration file either by passing command-line argument or by creating file called "config.yaml" at the root of the project.
```
stack run -- -c <config_path> 
```

To run tests, use can use:
```
stack test
```

## Migrations

All migrations are located at `server/migrations/` folder.

## Logging 
There are four levels of logging verbosity:
- Error - critical errors that lead to program or request handling halting
- Warn - recoverable errors
- Info - notifications about current actions
- Debug - debug information

## Test data

Test data is located in `/test-data/test.sql` to fill database with test data use `--test-data` or `-t` with path to `test.sql` file:

```
stack run -- -t <test data path> 
```

or

```
stack exec -- server-exe -t <test data path> 
```

WARNING: You can only apply test data once.

## CURLS

CURL scripts are located in `/scripts/` folder.

## Project structure 

- `server-lib`
  - `src/`
    - `DB.hs` - contains DB related helper functions
    - `Errors.hs` - contains server error definitions
    - `Router.hs` - contains type-level routing implementation
    - `Infrastructure.hs` - namespace with all necessary server-lib functions
    - `Utils.hs` - contains project utility functions.
    - `Types/` 
      - `TH.hs` - contains Template Haskell macros for specialized type creation
      - `Router.hs` - contains router related types
      - `DB.hs` - contains DB related types
      - `Lenses.hs` - contains some lens helpers
      - `Infrastructure` - namespace with all necessary server-lib types
      - `TH/`
        - `Classes.hs` - contains Template Haskell macros for typeclass definition and instances creation

- `server`
  - `src/`
    - `Utils.hs` - contains some utility functions
    - `Migration.hs` - contains applyMigrations function
    - `Logger.hs` - contains asynchronous logger implementation
    - `Effects.hs` - namespace with all effect typeclasses
    - `Effects/` - contains entity-specific effects definitions using typeclasses
      - `<entity>.hs` 
    - `Server.hs` - contains top level server API definition
    - `Server/` - contains API method implementations
      - `Auth.hs` - contains implementation of authentication and "/auth" API
      - `Errors.hs` - contains API method errors
      - `User.hs` - contains API definition for users
      - `User/` - contains API for users implementation 
        - `<entity>.hs`
      - `Admin.hs` - contains API definitions
      - `Admin/` - contains API for admins implementation
        - `<entity>.hs`
    - `Database/` - contains entity-specific database functions
      - `<entity>.hs` 
    - `Types/` 
      - `Auth.hs` - contains types used for authentification
      - `Environment.hs` - contains config, environment and base monad stack definitions
      - `Logger.hs` - contains logger implementation-related types
      - `Utils.hs` - contains types for utility functions
      - `<entity>.hs` - contains entity-specific type definitions
    
## API endpoints

Request examples may be found at folder `/scripts`.

### `/auth`

#### `POST /auth/create`

**Permission**: `None`

Endpoint for user creation.

Request body implements an interface:
```
interface ICreateUserRequest {
  name: string;
  surname: string;
  login: string;
  avatar: string?;      // ID of the picture in database
  password: string;
}
```

Response body implements an interface:
```
interface ICreateUserResponse {
  token: string;
  refreshToken: string;
  expires: timestamptz;
}
```

#### `POST /auth/login`

**Permission**: `None`

Endpoint for token retrieval.

Request body implements an interface:
```
interface ILoginRequest {
  login: string;
  password: string;
}
```

Response body implements an interface:
```
interface ILoginResponse {
  token: string;
  refreshToken: string;
  token: string;
}
```

#### `GET /auth/refresh`

**Permission**: `User`

Authorization header must contain refresh token.

Response body implements an interface:
```
interface IRefreshResponse {
  token: string;
  refreshToken: string;
  token: string;
}
```

### `/users`

#### `GET /users` 

**Permission**: `User`

Endpoint for getting current authenticated user.

Response body implements an interface:
```
interface IUserPayloadResponse {
  userId: string;
  name: string;
  surname: timestamptz;
  login: string;
  avatar?: string;
  createdAt: timestamptz;
  privileded: bool;
}
```

#### `DELETE /users/<user_id>` 

**Permission**: `Admin`

Endpoint for deleting users. Returns empty response body with the following codes:

| Code | Description |
| --- | --- |
| `204` | **Removed successfully** |
| `404` | **User not found** |
| `403` | **Not enough rights (attempt to delete an admin)** |
Warning: Deleting user also deletes all entities related to deleted user.

### `/pictures`

#### `POST /pictures`

**Permission**: `User`

Endpoint for posting picture to server. Accepts raw bytes request body with corresponding content-type header.

```
interface IPicturePayloadResponse {
  pictureId: string;
}
```

### `/categories`

#### `GET /categories/<category_id>` 

**Permission** `User`

Endpoint for getting category information.
Method accepts query parameters:
  - recursive: bool, default true

Response body implements an interface:
```
interface IGetCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```

#### `POST /categories`

**Permission** `User`

Endpoint for category creation.

Request body implements an interface:
```
interface ICreateCategoryRequest {
  title: string;
  parent?: string;
}
```

Response body implements an interface:
```
interface ICreateCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```

#### `DELETE /categories/<category_id>`

**Permission** `Admin`

Endpoint for deleting categories. Returns empty response body with the following codes:

| Code | Description |
| --- | --- |
| `204` | **Removed successfully** |
| `404` | **Category not found** |

#### `PUT /categories`

**Permission** `Admin`

Method accepts query parameters:
  - title: string
  - parent: string (parent category ID)

Response body implements an interface:
```
interface IUpdateCategoryResponse {
  categoryId: string;
  title: string;
  parent?: string;
}
```
