Haskell web application with following primary functionality:

 * a public form to request application user to speak on events
 * an admin page to get overview and manage these requests
 * notifying application user about deadlines and changes in these requests

## Documentation

Modules and many functions are annotated with [Haddock](https://www.haskell.org/haddock/) compatible comments.

It can be generated from repository with:
```
stack exec -- haddock --html src/**/*.hs --odir=doc
```

## Installation

### Quick start

[Stack](https://docs.haskellstack.org/en/stable/README/) is required to build the application.

```
# Create empty database
touch database.json

# Create directory for attachments and notifications
mkdir attachments notifications

# Install dependencies, compile and run application
stack run -- config_sample.ini
```

Example configuration is shown in [config_sample.ini](config_sample.ini).

 | Option | Description |
 |--------|-------------|
 | `main.appdir` | (optional) Application runtime directory; defaults to executable location directory |
 | `main.datadir` | Directory of application databases |
 | `main.host`, `main.port` | Listener address (optional - defaults to `127.0.0.1`) and port |
 | `main.url` | Public base URL of the application |
 | `emails.from_name`, `emails.from_email` | Name (optional) and address of mail source |
 | `emails.notifications_to_name`, `emails.notifications_to_email` | Name (optional) and address of notifications' email |
 | `smtp.host`, `smtp.port` | SMTP server host and port (optional - default value depends on TLS mode) used to send mail |
 | `smtp.tlsMode` | SMTP TLS mode<br>`"none"` - no TLS<br>`"ssl"` - connection level TLS<br>`"starttls"` - protocol level TLS |
 | `smtp.username`, `smtp.password` | (optional) username and password used to access SMTP server |
 | `gmail.username`, `gmail.password` | GMail username and password used to send event responses |
 | `gmaps.key` | Google Maps API key |

**Note:** If SMTP authentication is configured, it will be done using `PLAIN` authentication method.

## Customizing event form

The contents of event form is determined by [CommonMark](https://commonmark.org/) file [templates/form.md](templates/form.md). First line of the template is used as page title and ignored for form rendering. The input fields are specified by special code blocks encoding key-value pairs, each on its own line, separated by single space.

Field definition:

 | Key | Description |
 |-----|-------------|
 | `field` | Field ID - used to identify and match fields within the application. Some fields - specified by field ID - have special semantics/meaning as show in table below. |
 | `type` | *(optional)* Field type if applicable - every value except `email` is handled as `text` |
 | `name` | User readable string shown on admin page - used only for custom fields |
 | `section` | *(optional)* User readable section name shown on admin panel |
 | `label` | *(optional)* Label show to user - defaults to `name` |
 | `required` | *(optional)* Marks field as required |
 | `error` | *(optional)* Error message to display when any validations fail |
 | `repeated` | *(optional)* Makes field dynamically expanding and retracting list. If argument is `true`, `yes` or `1`, it also appends field number (as in list ordering) to field label |

Be aware that adjacent code blocks are merged unless there is something else between them. `&shy;` ([Soft hyphen](https://en.wikipedia.org/wiki/Soft_hyphen)) would do fine.

Special fields:

 | Field | Description |
 |-------|-------------|
 | `email` | Contact email used by application to send responses etc. |
 | `eventName` | Event name |
 | `eventUrl` | Event URL |
 | `arriveDate` | Start of invitation |
 | `leaveDate` | End of invitation |
 | `configurationDeadline` | Deadline by which user should make decision |
 | `eventLocation` | Event location (with Google Maps) |
 | `attachments` | Attachments |

Note that spacial fields might ignore some field definition keys (i.e. `email` field cannot be made repeating).

Full examplle can be found in [templates/form.md](templates/form.md).

## Customizing mail templates

There's two types of mail templates:
 - Event submission confirmation email (e.g. [confirmation.md](templates/confirmation.md)) and contain email subject template on first line.
 - Response templates - these start with 'response-' prefix (e.g. [response-accept.md](templates/response-accept.md)) and contain "summary" text on first line and subject template on second line.

Mail templates are also CommonMark [CommonMark](https://commonmark.org/) files. Templates have special syntax to inject values/variables:

```
Some $variable$
```

Following variables/placeholders are available:

 | Variable | Description |
 |----------|-------------|
 | `uuid` | UUID of invitation |
 | `editUrl` | Edit/view URL |
 | `email` | Invitation E-Mail |
 | `eventName` | Event name |
 | `eventUrl` | Event URL |
 | `eventLocation` | Event location name |
 | `eventLocationUrl` | Google Maps URL of event location |
 | `baseUrl` | Public base URL of the application |
 | `mailFrom` | E-Mail from which the mail is sent by application |
 | `mailFromName` | E-Mail name from which the mail is sent |
 | `notificationsTo` | E-Mail address to which notifications from application go |
 | `notificationsToName` | E-Mail name to which notifications go |
 | Other | Will be looked up from custom fields. Multiple/repeated fields are concatenated with `, ` |

**Note:** Variable escaping is not URL-sensible, i.e. using variables as URL components is not safe.

**Warning:** Since mail address and much of the content is determined by user input, make sure to include disclaimer in template or otherwise make sure malicious user cannot send scam emails.
