/*
 * (C) Copyright IBM Corp. 2016,2020
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking;

import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;
import com.fasterxml.jackson.core.JsonProcessingException;
import com.fasterxml.jackson.databind.JsonNode;
import com.fasterxml.jackson.databind.ObjectMapper;
import com.ibm.whc.deid.ObjectMapperFactory;
import com.ibm.whc.deid.configuration.MaskingConfiguration;
import com.ibm.whc.deid.providers.masking.fhir.MaskingProviderBuilder;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.utils.log.LogCodes;
import scala.Tuple2;
import scala.Tuple3;

public abstract class AbstractComplexMaskingProvider<K> extends AbstractMaskingProvider {
  /** */
  private static final long serialVersionUID = -7270189743655406461L;

  protected HashMap<String, List<String>> maskingAuditRecords = new HashMap<>();
  protected final Map<String, MaskingProviderBuilder> maskingProviderMap = new HashMap<>();

  public static final String DISABLE_TYPES_VALUE = "default";

  protected String keyForType;

  protected String certificateID;

  protected MaskingProviderFactory maskingProviderFactory;

  private final String prefixGUID;

  protected String identifier;

  public K mask(K obj) {
    return obj;
  }

  protected String getSubfieldName(String declaredName) {
    return prefixGUID + declaredName;
  }

  public AbstractComplexMaskingProvider() {
    // TODO: verify setting prefixGUID to empty string is reasonable
    this.prefixGUID = "";
  }

  public AbstractComplexMaskingProvider(String complexType,
      MaskingConfiguration maskingConfiguration) {
		this.prefixGUID = maskingConfiguration.getStringValue(complexType + ".prefixGUID");

		this.keyForType = DISABLE_TYPES_VALUE;
	}

    public AbstractComplexMaskingProvider(DeidMaskingConfig maskingConfiguration) {
    // TODO: verify setting prefixGUID to empty string is reasonable
    this.prefixGUID = "";

    this.keyForType = maskingConfiguration.getJson().getMessageTypeKey();

    // if no Key is being passed into json.messageType.key then set to
    // default
    if (this.keyForType == null || this.keyForType.isEmpty()) {
      this.keyForType = DISABLE_TYPES_VALUE;
    }
  }

  @Override
public String mask(String identifier) {
    return null;
  }

  /**
   * Reads masking configuration to get all the resources
   *
   * @param resource
   * @return
   */
  protected List<Tuple2<String, JsonNode>> maskResource(List<Tuple2<String, JsonNode>> resource) {

    MaskingProviderBuilder maskingProvider;

    Set<String> resourceTypes = new HashSet<>();
    List<Tuple3<String, JsonNode, String>> partitionedList = resource.stream().map(input -> {
      String resourceTypeName = "";
      if (!keyForType.equals(DISABLE_TYPES_VALUE)) {
        JsonNode resourceNode = input._2.get(keyForType);

        String resourceType = resourceNode.asText();
        resourceTypeName = resourceType.toLowerCase();

      } else {
        resourceTypeName = DISABLE_TYPES_VALUE;
      }
      resourceTypes.add(resourceTypeName);
      return new Tuple3<String, JsonNode, String>(input._1(), input._2(), resourceTypeName);
    }).collect(Collectors.toList());

    List<Tuple3<String, JsonNode, String>> returnList = new ArrayList<>();
    for (String resourceName : resourceTypes) {
      maskingProvider = maskingProviderMap.get(resourceName);
      List<Tuple3<String, JsonNode, String>> maskList = partitionedList.stream().filter(input -> {
        return input._3().equals(resourceName);
      }).collect(Collectors.toList());
      if (maskingProvider != null) {

        List<Tuple3<String, JsonNode, String>> maskedNode = maskingProvider.mask(maskList);
        maskingAuditRecords.put(identifier, maskingProvider.getMaskingAuditTrailList());
        returnList.addAll(maskedNode);
      } else {
        returnList.addAll(maskList);
      }
    }

    return returnList.stream().map(record -> {
      return new Tuple2<String, JsonNode>(record._1(), record._2());
    }).collect(Collectors.toList());
  }

  /*
   * The masking provider works on FHIR objects that look like this: {
   * "resourceType":"ContactPoint", "system":"email", "value":"bob@gmail.com", "use":"home" }
   */

  /**
   * Given a resource, return the masked value in the input matching the resource
   *
   * @param resource
   * @return
   */
  public List<Tuple2<String, JsonNode>> maskJsonNode(List<Tuple2<String, JsonNode>> resource) {
    List<Tuple2<String, JsonNode>> maskedValue = maskResource(resource);

    return maskedValue.stream().filter(input -> {
      if (input._2() == null)
        return false;
      return true;
    }).collect(Collectors.toList());
  }

  /**
   * Given a masking configuration, return the masked JSON and audit trail record
   *
   * @param line
   * @param jobId
   * @return
   */
  @Override
public List<Tuple2<String, String>> maskWithBatch(List<Tuple2<String, String>> batch,
      String jobId) {

  ObjectMapper mapper = ObjectMapperFactory.getObjectMapper();

    return maskJsonNode(batch.stream().map(input -> {
      try {
        return new Tuple2<String, JsonNode>(input._1, mapper.readTree(input._2));
      } catch (JsonProcessingException e) {
        log.logError(LogCodes.WPH1017E, e, "maskWithBatch", input._2());
      } catch (IOException e) {
        log.logError(LogCodes.WPH1017E, e, "maskWithBatch", input._2());
      }
      return null;
    }).collect(Collectors.toList())).stream().map(input -> {
      try {
        return new Tuple2<String, String>(input._1(), mapper.writeValueAsString(input._2()));
      } catch (JsonProcessingException e) {
        log.logError(LogCodes.WPH1013E, e);
      }
      return null;
    }).collect(Collectors.toList());
  }

  public List<String> getMaskingAuditListPerIdentifier(String identifier) {
    return maskingAuditRecords.get(identifier);
  }

  public String getIdentifier() {
    return identifier;
  }

  public void setIdentifier(String identifier) {
    this.identifier = identifier;
  }
}
