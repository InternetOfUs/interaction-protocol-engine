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

import javax.ws.rs.GET;
import javax.ws.rs.Path;
import javax.ws.rs.Produces;
import javax.ws.rs.core.MediaType;

import io.swagger.v3.oas.annotations.Operation;
import io.swagger.v3.oas.annotations.Parameter;
import io.swagger.v3.oas.annotations.media.Content;
import io.swagger.v3.oas.annotations.media.Schema;
import io.swagger.v3.oas.annotations.responses.ApiResponse;
import io.swagger.v3.oas.annotations.tags.Tag;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;
import io.vertx.ext.web.api.service.WebApiServiceGen;

/**
 * Resource to provide help about the api.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Path(Help.PATH)
@Tag(name = "Other")
@WebApiServiceGen
public interface Help {

  /**
   * The path to the help resources.
   */
  String PATH = "/help";

  /**
   * The address of this service.
   */
  String ADDRESS = "wenet_interaction_protocol_engine.api.help";

  /**
   * The path to the information of the API.
   */
  String INFO_PATH = "/info";

  /**
   * The path to the Open API description.
   */
  String OPENAPI_YAML_PATH = "/openapi.yaml";

  /**
   * The handler for the get information about the API.
   *
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path(INFO_PATH)
  @Operation(summary = "Get the information about the API", description = "Return the relevant information of the API implementation")
  @ApiResponse(responseCode = "200", description = "The API information", content = @Content(schema = @Schema(implementation = APIInfo.class)))
  @Produces(MediaType.APPLICATION_JSON)
  void getInfo(@Parameter(hidden = true, required = false) ServiceRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

  /**
   * The handler for the get open API description.
   *
   * @param context       of the request.
   * @param resultHandler to inform of the response.
   */
  @GET
  @Path(OPENAPI_YAML_PATH)
  @Operation(summary = "Get the Open API description", description = "Return the Open API description of this API")
  @ApiResponse(responseCode = "200", description = "The API description")
  @Produces("application/yaml")
  void getOpenApi(@Parameter(hidden = true, required = false) ServiceRequest context, @Parameter(hidden = true, required = false) Handler<AsyncResult<ServiceResponse>> resultHandler);

}
