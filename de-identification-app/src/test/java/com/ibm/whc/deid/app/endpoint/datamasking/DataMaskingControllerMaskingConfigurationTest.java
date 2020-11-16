/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.datamasking;

import static org.hamcrest.CoreMatchers.containsString;
import static org.hamcrest.CoreMatchers.startsWith;
import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.post;
import static org.springframework.test.web.servlet.result.MockMvcResultHandlers.print;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.content;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.nio.file.Files;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.List;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.result.MockMvcResultHandlers;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import org.springframework.web.context.WebApplicationContext;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.fasterxml.jackson.databind.node.ArrayNode;
import com.fasterxml.jackson.databind.node.ObjectNode;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.exception.DeidException;
import com.ibm.whc.deid.shared.pojo.config.ConfigSchemaType;
import com.ibm.whc.deid.shared.pojo.masking.DataMaskingModel;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other active profile
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = { "test" })
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class DataMaskingControllerMaskingConfigurationTest {

	private final String basePath = "/api/v1";

	private MockMvc mockMvc;

	private static final Logger log = LoggerFactory.getLogger(DataMaskingControllerMaskingConfigurationTest.class);

	@Autowired
	private WebApplicationContext wac;

	@Before
	public void setup() throws DeidException {

		this.mockMvc = MockMvcBuilders.webAppContextSetup(wac).build();
	}

	/**
	 * Localization should not fail if Database or tables are missing. It should use
	 * the default list of providers
	 * 
	 * @throws Exception
	 */
	@Test
	public void testLocalizationMaskingConfig() throws Exception {
		String data = new String(
				Files.readAllBytes(Paths.get(getClass().getResource("/masking/data/fhir_location2.json").toURI())));
		String config = new String(Files
				.readAllBytes(Paths.get(getClass().getResource("/config/fhir/localization_masking.json").toURI())));
		ObjectMapper mapper = new ObjectMapper();
		ObjectNode rootNode = mapper.createObjectNode();
		ArrayNode dataNode = rootNode.putArray("data");
		dataNode.add(data);
		rootNode.put("config", config);
		rootNode.put("schemaType", (String) "FHIR");
		String request = mapper.writeValueAsString(rootNode);

		log.info(request);
		this.mockMvc
				.perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_UTF8_VALUE)
						.content(request))
				.andDo(print()).andExpect(status().is2xxSuccessful()).andDo(MockMvcResultHandlers.print());
	}

	/**
	 * Masking rules no longer support "<RULE>:null"
	 * Returns error in return body 
	 * @throws Exception
	 */
	@Test
	public void testInvalidMaskingConfig() throws Exception {
		String data = new String(
				Files.readAllBytes(Paths.get(getClass().getResource("/masking/data/simple_fhir.json").toURI())));
		String config = new String(Files
				.readAllBytes(Paths.get(getClass().getResource("/config/fhir/invalid_masking_config.json").toURI())));
		ObjectMapper mapper = new ObjectMapper();
		ObjectNode rootNode = mapper.createObjectNode();
		ArrayNode dataNode = rootNode.putArray("data");
		dataNode.add(data);
		rootNode.put("config", config);
		rootNode.put("schemaType", (String) "FHIR");
		String request = mapper.writeValueAsString(rootNode);

		log.info(request);
		this.mockMvc
				.perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_UTF8_VALUE)
						.content(request))
				.andDo(print()).andExpect(status().isBadRequest()).andDo(MockMvcResultHandlers.print())
				.andExpect(content().string("The JSON masking rule does not refer to a valid rule: HASH:null. There are 1 invalid rules."));
	}
	
	/** 
	 * Missing Message Type should not break application
	 * It should return user the un-masked output in the return body
	 * @throws Exception
	 */
	@Test
	public void testMaskingConfigMissingMessageType() throws Exception {
		String data = new String(
				Files.readAllBytes(Paths.get(getClass().getResource("/masking/data/fhir_location1.json").toURI())));
		String config = new String(Files
				.readAllBytes(Paths.get(getClass().getResource("/config/fhir/invalid_masking_config_missing_messageType.json").toURI())));
		ObjectMapper mapper = new ObjectMapper();
		ObjectNode rootNode = mapper.createObjectNode();
		ArrayNode dataNode = rootNode.putArray("data");
		dataNode.add(data);
		rootNode.put("config", config);
		rootNode.put("schemaType", (String) "FHIR");
		String request = mapper.writeValueAsString(rootNode);

		log.info(request);
		this.mockMvc
				.perform(post(basePath + "/deidentification").contentType(MediaType.APPLICATION_JSON_UTF8_VALUE)
						.content(request))
				.andDo(print()).andExpect(status().is2xxSuccessful()).andDo(MockMvcResultHandlers.print());
		}
}
