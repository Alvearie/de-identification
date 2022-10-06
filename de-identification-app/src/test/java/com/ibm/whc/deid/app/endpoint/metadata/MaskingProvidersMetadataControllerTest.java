/*
 * (C) Copyright Merative 2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.app.endpoint.metadata;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNotNull;
import static org.junit.Assert.assertTrue;
import static org.junit.Assert.fail;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.status;
import java.util.ArrayList;
import java.util.HashSet;
import org.junit.Before;
import org.junit.Test;
import org.junit.runner.RunWith;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.mock.web.MockHttpServletResponse;
import org.springframework.test.context.ActiveProfiles;
import org.springframework.test.context.junit4.SpringRunner;
import org.springframework.test.web.servlet.MockMvc;
import org.springframework.test.web.servlet.request.MockMvcRequestBuilders;
import org.springframework.test.web.servlet.setup.MockMvcBuilders;
import com.fasterxml.jackson.databind.JsonNode;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.app.endpoint.Application;
import com.ibm.whc.deid.shared.pojo.masking.MaskingProviderType;

@RunWith(SpringRunner.class)
// force using a test profile to avoid using any other profiles that may have
// properties set to use production db
// we do not have a real application-test.properties.
@ActiveProfiles(profiles = {"test"})
@AutoConfigureMockMvc
@SpringBootTest(classes = Application.class)
public class MaskingProvidersMetadataControllerTest {

  private static final String basePath = "/api/v1";

  private MockMvc mockMvc;

  @Before
  public void setup() throws Exception {
    this.mockMvc =
        MockMvcBuilders
            .standaloneSetup(
                new MaskingProvidersMetadataController(new MaskingProvidersMetadataService()))
            .build();
  }

  @Test
  public void runProvidersMetadata() {
    try {
      MockHttpServletResponse response =
          mockMvc.perform(MockMvcRequestBuilders.get(basePath + "/maskingproviders"))
              .andExpect(status().isOk()).andReturn().getResponse();

      assertTrue(response.getHeader("Content-Type").startsWith("application/json"));

      byte[] bytes = response.getContentAsByteArray();
      // can't deserialize into ProvidersMetadata as some of the imbedded objects are polymorphic
      // and we do not have custom deserializers defined
      JsonNode root = ObjectMapperFactory.getObjectMapper().readTree(bytes);
      JsonNode providers = root.get("providers");
      assertTrue(providers.isArray());
      // The FHIR_MORTALITY_DEPENDENCY provider is currently undocumented and removed from metadata
      // TODO reinstate or completely remove
      int targetCount = MaskingProviderType.values().length - 1;
      assertEquals(targetCount, providers.size());
      HashSet<String> names = new HashSet<>(targetCount * 2);
      final ArrayList<String> container = new ArrayList<>(1);
      container.add("");
      providers.forEach(t -> {
        assertTrue(t.isObject());
        JsonNode nameNode = t.get("name");        
        assertNotNull(nameNode);
        assertTrue(nameNode.isValueNode());
        String name = nameNode.asText();
        assertTrue(name.length() > 0);
        assertTrue(name.compareToIgnoreCase(container.get(0)) >= 0);  // ascending alphabetic order
        container.set(0, name);
        names.add(name);
      });
      for (MaskingProviderType type : MaskingProviderType.values()) {
        // TODO reinstate or completely remove the FHIR Mortality provider
        if (type != MaskingProviderType.FHIR_MORTALITY_DEPENDENCY) {
          assertTrue(names.remove(type.getIdentifier()));
        }
      }

    } catch (Exception e) {
      e.printStackTrace();
      fail(e.getClass().getName() + e.getMessage());
    }
  }
}
