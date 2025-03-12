const originalEvent = assembleEvent();

/**
 * Enables save button
 */
function enableSave() {
	const originalEventString = JSON.stringify({ ...originalEvent, timestamp: undefined });
	const currentEventString = JSON.stringify({ ...assembleEvent(), timestamp: undefined });
	const button = document.querySelector('#save-event');
	button.disabled = !fileAttachments.length && originalEventString === currentEventString;
}


/**
 * Uploads attachments to server
 * @returns {string[]} List of attachment UUID-s
 */
function uploadAttachments() {
	const body = new FormData;

	fileAttachments.forEach(attachment => {
		body.append('attachment', attachment);
	});

	return fetch('attachment', {
		method: 'POST',
		body
	}).then(response => {
		if (response.ok) {
			return response.json();
		} else {
			throw new Error('Bad status code');
		}
	});
}


/**
 * Tryies to save event to server. Handles success and error conditions
 */
function saveEvent(event) {
	const isNew = event.uuid === '00000000-0000-0000-0000-000000000000';
	return fetch(isNew ? '' : event.uuid, {
		method: 'POST',
		headers: {
			'content-type': 'application/json; charset=utf-8'
		},
		body: JSON.stringify(event)
	}).then(response => {
		// TODO: some nicer notification UX, redirection, additinal information, etc.
		if (response.ok) {
			return response.json();
		} else {
			throw new Error('Bad status code');
		}
	}).then(body => {
		if (isNew) {
			window.location = encodeURIComponent(body.uuid) + '/submitted'
				+ '?token=' + encodeURIComponent(body.token);
		} else {
			$.notify({ message: 'Changes have been saved' }, { type: 'success' });
			setTimeout(() => {
				window.location.href = window.location.href
			}, 2000);
		}
	});
}


/**
 * Assembles user input into event object
 */
function assembleEvent() {

	const presetFields = [
		'uuid',
		'token',
		'email',
		'eventName',
		'eventUrl',
		'eventDescription',
		'arriveDate',
		'leaveDate',
		'eventLocation',
		'eventLocationCoordinates',
		'date',
		'time',
		'callInUrl',
		'confirmationDeadline',
		'attachments'
	];

	const event = {
		timestamp: new Date().toISOString(),
		summary: '',
		attachments: [],
		fields: []
	};

	const inputs = document.querySelectorAll('input[name], textarea[name]')

	for (const element of inputs) {
		if (element.name === 'attachments') {
			if (element.type === 'hidden') {
				event.attachments.push(element.value);
			}
		} else if (presetFields.includes(element.name)) {
			const tab = element.closest('.tab-pane');
			if (!tab || tab.classList.contains('active')) {
				event[element.name] = element.value;
			}
		} else if (element.name !== 'eventType') {
			const repeatedContainer = element.parentNode.parentNode;
			// skip empty repeated fields
			if(repeatedContainer.hasAttribute('data-repeated') && !element.value) {
				continue;
			}

			const field = {
				field: element.name,
				name: element.getAttribute('data-name'),
				value: element.value,
				section:element.getAttribute('data-section')
			};
			event.fields.push(field);
		}
	}

	return event;
}


/**
 * Validate user inputs
 */
function validateFields() {
	removeAllFieldErrors()

	let isValid = true
	document.querySelectorAll('input[name], textarea[name]').forEach(input => {
		const tab = input.closest('.tab-pane');
		const active = !tab || tab.classList.contains('active')

		if (active && !input.checkValidity()) {
			const message = input.getAttribute('data-error') || input.validationMessage || 'Required field';
			addFieldError(input.parentElement, message)
			isValid = false
		}
	})

	// Scroll to first error
	const firstError = document.querySelector('.field-error')
	if (firstError)
		firstError.parentElement.querySelector('input').focus()

	return isValid
}

function addFieldError(group, msg) {
	const p = document.createElement('p')
	p.classList.add('field-error')
	p.innerHTML = msg
	p.style.color = "#f44336"
	group.appendChild(p)
}

function removeAllFieldErrors() {
	document.querySelectorAll('.field-error').forEach(el => el.remove())
}

/**
 * Expand reapeated field input
 */
function expandList(list) {
	const formGroups = list.children
	const hasEmptyField = Array.from(formGroups)
		.some(fg => !fg.querySelector('input').value);

	if (!hasEmptyField) {
		const newFormGroup = formGroups[0].cloneNode(true);
		const tooltip = newFormGroup.querySelector('.input-help')
		if (tooltip) tooltip.remove()
		const newInput = newFormGroup.querySelector('input');
		newInput.value = ""
		newInput.addEventListener('change', enableSave);
		newInput.addEventListener('input', enableSave);
		const label = newFormGroup.querySelector('label');
		if (label) {
			const hasSuffix = list.getAttribute('data-repeated') === 'True';
			const labelText = list.getAttribute('data-label') || '';
			const suffix = hasSuffix ? String(formGroups.length + 1) : ''
			label.innerText = labelText + suffix
		}
		list.appendChild(newFormGroup)
		attachExpandingList(list)
	}
}

function contractList(list) {
	const formGroups = list.children
	const emptyFG = Array.from(formGroups)
		.filter(fg => !fg.querySelector('input').value)

	if (emptyFG.length > 1) {
		emptyFG.pop()
		emptyFG.forEach(fg => fg.remove())

		// Recalculate label numbering
		if (list.getAttribute('data-repeated') === 'True') {
			const labelText = list.getAttribute('data-label') || '';
			let count = 1;
			for (const fg of list.children) {
				const label = fg.querySelector('label');
				if (label) {
					fg.querySelector('label').innerHTML = labelText + count++;
				}
			}
		}
	}
}

function attachExpandingList(list) {
	for (const fg of list.children) {
		fg.querySelector('input').addEventListener('keyup', e => expandList(list));
		fg.querySelector('input').addEventListener('blur', e => contractList(list));
	}
}


/**
 * Main
 */

for(const repeatedField of document.querySelectorAll('[data-repeated]')) {
	attachExpandingList(repeatedField);
}

document.querySelector('#save-event').addEventListener('click', e => {
	e.preventDefault();
	if (validateFields()) {
		const button = document.querySelector('#save-event');
		button.disabled = true;
		const loadingButton = button.cloneNode(false);
		loadingButton.innerText = button.getAttribute('data-loading-label');
		button.parentNode.replaceChild(loadingButton, button);
		uploadAttachments()
			.then(attachments => {
				const event = assembleEvent();
				event.attachments.push(...attachments);
				return saveEvent(event);
			})
			.catch(e => {
				$.notify({ message: 'Failed to save invition' }, { type: 'danger' });
				console.error(e);
				loadingButton.parentNode.replaceChild(button, loadingButton);
				button.disabled = false;
			});
	}
});

$('.datetimepicker').datetimepicker({
	format: 'YYYY-MM-DD',
	icons: {
		time: "fa fa-clock-o",
		date: "fa fa-calendar",
		up: "fa fa-chevron-up",
		down: "fa fa-chevron-down",
		previous: 'fa fa-chevron-left',
		next: 'fa fa-chevron-right',
		today: 'fa fa-screenshot',
		clear: 'fa fa-trash',
		close: 'fa fa-remove'
	}
}).on('dp.change', () => {
	enableSave();
});

const arriveDate = $('.datetimepicker[name=arriveDate]'),
      leaveDate = $('.datetimepicker[name=leaveDate]');

if(arriveDate.length && leaveDate.length) {
	arriveDate.on('dp.change', e => {
		leaveDate.data('DateTimePicker').minDate(e.date);
		leaveDate.parent().addClass('is-filled');
	});

	const arriveDateValue = arriveDate.data('DateTimePicker').date();
	if(arriveDateValue) {
		leaveDate.data("DateTimePicker").minDate(arriveDateValue);
	}
}

/**
 * Since the UI framework doesn't support dynamic labels on datefields, we simulate it.
 */
document.querySelectorAll('.datetimepicker').forEach(picker => {
	picker.addEventListener('focus', e => {
		e.target.parentElement.classList.add('is-filled');
	});
});

/**
 * Enable save changes button when any field changes
 */

for(const input of document.querySelectorAll('input, textarea')) {
	input.addEventListener('input', enableSave);
	input.addEventListener('change', enableSave);
}

/*
* Enable tooltips
* */
$('[data-toggle=tooltip]').tooltip()
