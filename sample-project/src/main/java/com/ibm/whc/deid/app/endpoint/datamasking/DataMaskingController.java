/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestHeader;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;

import io.undertow.util.BadRequestException;

@RestController
@RequestMapping("/api/v1")
public class DataMaskingController {
	@PostMapping("/deidentification")
	ResponseEntity<?> maskJson(@RequestBody String maskRequest,
			@RequestHeader(value = "tenant", required = false) String tenant)

			throws BadRequestException {
		String s1 = "EA72C79594296E45B8C2A296644D988581F58CFAC6601D122ED0A8BD7C02E8BF";
		String s2 = "96BD923157C731249A40C36426FC326062AD3B2904ED6792B3F404F223D35651";
		String s3 = "9345A35A6FDF174DFF7219282A3AE4879790DBB785C70F6FFF91E32FAFD66EAB";
		return new ResponseEntity<>(s1 + s2 + s3, HttpStatus.OK);
	}

}
