function loadingButton(button, text) {
	const buttons = [...button.parentNode.querySelectorAll('button')];
	buttons.forEach(button => button.disabled = true);
	const loadingButton = button.cloneNode(false);
	loadingButton.innerText = text;
	button.parentNode.replaceChild(loadingButton, button);

	return function enableSave() {
		loadingButton.parentNode.replaceChild(button, loadingButton);
		buttons.forEach(button => button.disabled = false);
	}
}


function postDraft(form) {
	const resultContainer = form.querySelector('.response-save-result')
	const summary = form.querySelector('input[name=summary]').value;
	const subject = form.querySelector('input[name=subject]').value;
	const content = form.querySelector('textarea[name=content]').value;
	const enableSave = loadingButton(form.querySelector('button.post-draft'), 'posting');

	resultContainer.innerHTML = '';

	fetch('admin/event/' + window.eventUuid + '/response', {
		method: 'POST',
		headers: { 'content-type': 'application/json' },
		body: JSON.stringify({ summary, draft: { subject, content } })
	})
		.then(response => {
			if(response.ok) {
				return response.json();
			} else {
				throw new Error('Bad status code');
			}
		})
		.then(body => {
			const link = $('<div>').text(body.draftUrl).html();
			resultContainer.innerHTML = '<div class="alert alert-success" role="alert" id="draft-save-success">A <a href="' + link + '" style="text-decoration: underline">draft</a> has been created.</div>';
			enableSave();
		})
		.catch(e => {
			$.notify({ message: 'Network error occurred while submitting response.' }, { type: 'danger' });
			enableSave();
			throw e;
		});
}


function apply(form) {
	const resultContainer = form.querySelector('.response-save-result')

	const summary = form.querySelector('input[name=summary]').value;
	const enableSave = loadingButton(form.querySelector('button.apply'), 'applying');

	fetch('admin/event/' + window.eventUuid + '/response', {
		method: 'POST',
		headers: { 'content-type': 'application/json' },
		body: JSON.stringify({ summary, draft: null })
	})
		.then(response => {
			if(response.ok) {
				resultContainer.innerHTML = '<div class="alert alert-success" role="alert" id="draft-save-success">Response has been saved</div>';
				enableSave();
			} else {
				throw new Error('Bad status code');
			}
		})
		.catch(e => {
			$.notify({ message: 'Network error occurred while saving summary.' }, { type: 'danger' });
			enableSave();
			throw e;
		});
}


function ignoreEvent() {
	const result = confirm("Ignore event?");
	if(!result) {
		return;
	}

	fetch('admin/event/' + window.eventUuid + '/response', {
		method: 'POST',
		headers: { 'content-type': 'application/json' },
		body: JSON.stringify({ summary: "ignore", draft: null })
	})
		.then(response => {
			if(!response.ok) {
				throw new Error('Bad status code');
			}
		})
		.catch(e => {
			$.notify({ message: 'Network error occurred while submitting response.' }, { type: 'danger' });
			throw e;
		});
}

$('.nav-item').click(() => {
	$('.response-save-result').html('');
});

$('button.post-draft').click(e => {
	const form = e.target.parentNode;
	postDraft(form);
});

$('button.apply').click(e => {
	const form = e.target.parentNode;
	apply(form);
});
