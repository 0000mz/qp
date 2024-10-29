run: *
	cargo build
	ICED_BACKEND=tiny-skia target/debug/qp
