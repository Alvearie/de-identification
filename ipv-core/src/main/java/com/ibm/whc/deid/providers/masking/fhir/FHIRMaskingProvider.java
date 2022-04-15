/*
 * (C) Copyright IBM Corp. 2016,2022
 *
 * SPDX-License-Identifier: Apache-2.0
 */
package com.ibm.whc.deid.providers.masking.fhir;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Collections;
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
import com.ibm.whc.deid.providers.masking.ComplexMaskingProvider;
import com.ibm.whc.deid.providers.masking.MaskingProviderFactory;
import com.ibm.whc.deid.pruners.GlobalProcessorFactory;
import com.ibm.whc.deid.shared.exception.KeyedIllegalArgumentException;
import com.ibm.whc.deid.shared.exception.KeyedRuntimeException;
import com.ibm.whc.deid.shared.pojo.config.DeidMaskingConfig;
import com.ibm.whc.deid.shared.pojo.config.GlobalProcessorConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonConfig;
import com.ibm.whc.deid.shared.pojo.config.json.JsonMaskingRule;
import com.ibm.whc.deid.shared.pojo.masking.ReferableData;
import com.ibm.whc.deid.shared.pojo.masking.ReferableNode;
import com.ibm.whc.deid.utils.log.LogCodes;
import com.ibm.whc.deid.utils.log.Messages;

public class FHIRMaskingProvider implements ComplexMaskingProvider, Serializable {

  private static final long serialVersionUID = 1L;

  public static final String DISABLE_TYPES_VALUE = "default";

  private final GlobalProcessorConfig gpConfig;
  private final Map<String, MaskingProviderBuilder> maskingProviderMap = new HashMap<>();
  private String keyForType;

  public FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration,
      MaskingProviderFactory maskingProviderFactory, String tenantId) {
    this(maskingConfiguration, maskingConfiguration.isDefaultNoRuleResolution(),
        maskingProviderFactory, "/fhir/", null, tenantId);
  }

  public FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration,
      MaskingProviderFactory maskingProviderFactory, GlobalProcessorConfig gpConfig,
      String tenantId) {
    this(maskingConfiguration, maskingConfiguration.isDefaultNoRuleResolution(),
        maskingProviderFactory, "/fhir/", gpConfig, tenantId);
  }

  protected FHIRMaskingProvider(DeidMaskingConfig maskingConfiguration, boolean defNoRuleRes,
      MaskingProviderFactory maskingProviderFactory, String basePathPrefix,
      GlobalProcessorConfig gpConfig, String tenantId) {

    this.gpConfig = gpConfig;

    this.keyForType = maskingConfiguration.getJson().getMessageTypeKey();
    // if no Key is being passed into json.messageTypeKey then set to default
    if (this.keyForType == null || this.keyForType.trim().isEmpty()) {
      this.keyForType = DISABLE_TYPES_VALUE;
    }
    // if using "default" message type, given message types list is ignored per documentation
    List<String> messageTypes;
    if (DISABLE_TYPES_VALUE.equals(this.keyForType)) {
      messageTypes = new ArrayList<>();
      messageTypes.add(DISABLE_TYPES_VALUE);
    } else {
      messageTypes = maskingConfiguration.getJson().getMessageTypes();
    }

    for (String type : messageTypes) {
      String basePath = basePathPrefix + type;
      List<FHIRResourceField> ruleAssignments =
          loadRulesForResource(type, maskingConfiguration, basePathPrefix);
      FHIRResourceMaskingConfiguration resourceConfiguration =
          new FHIRResourceMaskingConfiguration(basePath, ruleAssignments);
      this.maskingProviderMap.put(type.toLowerCase(),
          new MaskingProviderBuilder(resourceConfiguration, maskingConfiguration, defNoRuleRes,
              maskingProviderFactory, tenantId));
    }
  }

  /**
   * Load the rule assignments applicable for the given message type.
   *
   * @param resourceName the message type for which rules are obtained
   * @param maskingConfiguration the masking configuration
   * 
   * @return the applicable rule assignments
   */
  public static List<FHIRResourceField> loadRulesForResource(String resourceName,
      DeidMaskingConfig maskingConfiguration, String bashPathPrefix) {
    String matchPrefix = bashPathPrefix + resourceName + "/";
    List<FHIRResourceField> values = Collections.emptyList();
    JsonConfig jsonConfig = maskingConfiguration.getJson();
    if (jsonConfig != null) {
      List<JsonMaskingRule> ruleAssignments = jsonConfig.getMaskingRules();
      if (ruleAssignments != null && !ruleAssignments.isEmpty()) {
        values = new ArrayList<>(ruleAssignments.size());
        for (JsonMaskingRule assignment : ruleAssignments) {
          if (assignment.getJsonPath().startsWith(matchPrefix)) {
            values.add(new FHIRResourceField(assignment.getJsonPath(), assignment.getRule()));
          }
        }
      }
    }
    return values;
  }

  /**
   * Entry point for the JSON-based masking drivers (ComplexMaskingProvider).
   */
  @Override
  public List<ReferableData> maskWithBatch(List<ReferableData> payloadData, String jobId) {
    ObjectMapper objectMapper = ObjectMapperFactory.getObjectMapper();

    // convert to JSON objects
    List<ReferableNode> toMask = payloadData.stream().map(input -> {
      JsonNode node = null;
      try {
        node = objectMapper.readTree(input.getData());
      } catch (JsonProcessingException e) {
        throw new KeyedIllegalArgumentException(LogCodes.WPH1026E,
            Messages.getMessage(LogCodes.WPH1026E, input.getIdentifier(), e.getMessage()), e);
      }
      return new ReferableNode(input.getIdentifier(), node);
    }).collect(Collectors.toList());

    // call the global processor, if configured
    if (this.gpConfig != null
        && GlobalProcessorConfig.DEFAULT_RULE_SET.equals(this.gpConfig.getRuleSet())) {
      toMask = new GlobalProcessorFactory().getGlobalProcessor().processBatch(toMask);
    }

    // mask each JSON document
    List<ReferableNode> haveMasked = maskResources(toMask);

    // filter out any null results
    haveMasked = haveMasked.stream().filter(maskedResource -> maskedResource.getNode() != null)
        .collect(Collectors.toList());

    // convert masked JSON back to string
    List<ReferableData> toReturn = toMask.stream().map(input -> {
      ReferableData serializedNode = null;
      try {
        serializedNode = new ReferableData(input.getIdentifier(),
            objectMapper.writeValueAsString(input.getNode()));
      } catch (JsonProcessingException e) {
        // this is unlikely to occur
        throw new KeyedRuntimeException(LogCodes.WPH1013E,
            Messages.getMessage(LogCodes.WPH1013E, e.getMessage()), e);
      }
      return serializedNode;
    }).collect(Collectors.toList());

    return toReturn;
  }

  /**
   * Reads masking configuration to get all the resources
   *
   * @param resource
   * @return
   */
  protected final List<ReferableNode> maskResources(List<ReferableNode> resources) {

    Set<String> resourceTypes = new HashSet<>();
    List<MaskingProviderBuilder.MaskingResource> partitionedList = resources.stream().map(input -> {
      String resourceTypeName = "";
      if (!keyForType.equals(DISABLE_TYPES_VALUE)) {
        JsonNode resourceNode = input.getNode().get(keyForType);
        if (resourceNode != null && resourceNode.isValueNode() && !resourceNode.isNull()) {
          String resourceType = resourceNode.asText();
          resourceTypeName = resourceType.toLowerCase();
        }
      } else {
        resourceTypeName = DISABLE_TYPES_VALUE;
      }
      resourceTypes.add(resourceTypeName);
      return new MaskingProviderBuilder.MaskingResource(input.getIdentifier(), input.getNode(),
          resourceTypeName);
    }).collect(Collectors.toList());

    List<MaskingProviderBuilder.MaskingResource> finishedList =
        new ArrayList<>(partitionedList.size());
    MaskingProviderBuilder maskingProviderBuilder;
    for (String resourceName : resourceTypes) {
      List<MaskingProviderBuilder.MaskingResource> maskingInList =
          partitionedList.stream().filter(input -> {
            return input.getResourceType().equals(resourceName);
          }).collect(Collectors.toList());
      maskingProviderBuilder = maskingProviderMap.get(resourceName);
      if (maskingProviderBuilder != null) {
        List<MaskingProviderBuilder.MaskingResource> maskingOutList =
            maskingProviderBuilder.orchestrateMasking(maskingInList);
        finishedList.addAll(maskingOutList);
      } else {
        finishedList.addAll(maskingInList);
      }
    }

    return finishedList.stream().map(record -> {
      return new ReferableNode(record.getIdentifier(), record.getJsonNode());
    }).collect(Collectors.toList());
  }
}
