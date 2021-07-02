/*
 * -----------------------------------------------------------------------------
 *
 * Copyright 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import eu.internetofus.common.model.Model;
import eu.internetofus.common.model.ReflectionModel;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * A model with information about the API.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(name = "Info", description = "Provide the version information of the API")
public class APIInfo extends ReflectionModel implements Model {

  /**
   * The current version of the API.
   */
  @Schema(description = "Contain the name of the API", example = "wenet/interaction-protocol-engine")
  public String name;

  /**
   * The current version of the API.
   */
  @Schema(description = "Contain the implementation version number of the API", example = "1.0.0")
  public String apiVersion;

  /**
   * The current version of the software.
   */
  @Schema(description = "Contain the implementation version number of the software", example = "1.0.0")
  public String softwareVersion;

  /**
   * The current vendor of the API.
   */
  @Schema(description = "Contain information of the organization that has implemented the API", example = "UDT-IA, IIIA-CSIC")
  public String vendor;

  /**
   * The current vendor of the API.
   */
  @Schema(description = "Contain information of the license of the API", example = "MIT")
  public String license;

  /**
   * Create a new version.
   */
  public APIInfo() {

  }
}
