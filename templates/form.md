Lorem ipsum dolor sit amet, consectetur adipiscing elit. Ut ac odio maximus, pellentesque nisl at, tempor metus. Aliquam erat volutpat. Pellentesque efficitur nunc nunc, sed imperdiet magna bibendum eget. Fusce id felis ut lorem mollis accumsan. Curabitur placerat malesuada ipsum, ut gravida nunc feugiat eu. Donec varius velit in orci pellentesque lacinia. Cras in est semper, placerat velit vitae, eleifend nisi. In maximus sem ac mauris vehicula bibendum. Lorem ipsum dolor sit amet, consectetur adipiscing elit. Integer cursus quis metus et fringilla. Pellentesque tristique posuere libero, at sagittis ipsum varius sit amet. Vestibulum bibendum orci nec dui tincidunt varius nec et sem. Nulla at tortor metus. Nam vitae aliquam justo. Sed pharetra augue id dapibus condimentum.


## Existing invitations

&shy;

    calendar

## Invitation Form

<span class="color-red">Required fields marked by \*</span>

&shy;

    field email
    name Email address
    label Email address
    required
    error Must be a valid email

### Contact details

    field contactName
    name Contact Name
    label Full name of contact person
    required
    error Field is required
    section Contact Information

&shy;

    field contactEmail
    type email
    name Contact Email
    label Contact Email (if it does not match with email above)
    section Contact Information
    error Must be a valid email

&shy;

    field contactPhone
    name Contact Phone
    label Contact Information
    section Contact Information

### Event Information

    field eventName
    name Event Name
    required
    error Field is required

&shy;

    field eventUrl
    name Event URL
    label Event Webpage (URL)

&shy;

    field eventDescription
    name Event Description

### Event Type

    if onsite
    onsite-label Physical event
    online-label Virtual event

&shy;

### Physical Event Info

    field arriveDate
    name Arrive date
    required
    error Field is required
    tooltip Arrive date

&shy;

    field leaveDate
    name Leave date
    required
    error Field is required
    tooltip Leave date

&shy;

    field confirmationDeadline
    name Confirmation deadline
    label Confirmation deadline
    required
    error Field is required
    tooltip Confirmation deadline

&shy;

    field eventLocation
    name Location
    required
    error Field is required
    tooltip Location

&shy;

    else

&shy;

### Virtual Event Info

    field date
    name Date
    required
    error Field is required

&shy;

    field time
    name Time in UTC
    error Field is required
    tooltip Time in UTC

&shy;

    field confirmationDeadline
    name Confirmation deadline
    label Confirmation deadline
    required
    error Field is required
    tooltip Confirmation deadline

&shy;

    field callInUrl
    name Call-in URL
    error Field is required

&shy;

&shy;

    end

### Audience Information

    field audienceSize
    name Expected number of attendees
    tooltip Expected number of attendees

&shy;

    field audienceDemographic
    name Audience demographic
    tooltip Audience demographic

### Other speakers

    field speakers
    name Speaker
    label Wikipedia page of key speaker #
    repeated True

### Attachments

    field attachments
