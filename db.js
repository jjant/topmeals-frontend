// Usa esto: https://www.json-generator.com/

generator = [
	'{{repeat(5, 7)}}',
	{
		_id: '{{objectId()}}',
		calories: '{{integer(20, 400)}}',
		name: '{{lorem(1, "paragraphs")}}',
		datetime:
			'{{date(new Date(2014, 0, 1), new Date(), "YYYY-MM-ddThh:mm:ss Z")}}'
	}
];

// Y despues metelo aca https://www.mockable.io/
