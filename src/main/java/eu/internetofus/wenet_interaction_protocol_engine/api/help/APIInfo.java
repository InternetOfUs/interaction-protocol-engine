/*
 * -----------------------------------------------------------------------------
 *
 * Copyright (c) 2019 - 2022 UDT-IA, IIIA-CSIC
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in all
 * copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 *
 * -----------------------------------------------------------------------------
 */
package eu.internetofus.wenet_interaction_protocol_engine.api.help;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.ReflectionModel;
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
