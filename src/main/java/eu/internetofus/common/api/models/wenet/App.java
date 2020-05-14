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
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
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

package eu.internetofus.common.api.models.wenet;

import java.util.List;

import com.fasterxml.jackson.databind.annotation.JsonDeserialize;

import eu.internetofus.common.api.models.JsonObjectDeserializer;
import eu.internetofus.common.api.models.Model;
import io.swagger.v3.oas.annotations.media.ArraySchema;
import io.swagger.v3.oas.annotations.media.Schema;

/**
 * The application of a WeNet use case scenario.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@Schema(hidden = true, name = "App", description = "The app of a wenet use case scenario.")
public class App extends Model {

	/**
	 * The instant of the creation.
	 */
	@Schema(description = "The UTC epoch timestamp representing the account creation instant.", example = "1563871899")
	public long creationTs;

	/**
	 * The instant of the last update.
	 */
	@Schema(description = "The UTC epoch timestamp representing the last update instant.", example = "1563898764")
	public long lastUpdateTs;

	/**
	 * The identifier of the application.
	 */
	@Schema(description = "The Id of the wenet app.", example = "3e557acc-e846-4736-8218-3f64d8e68d8c")
	public String appId;

	/**
	 * The identifier of the application.
	 */
	@Schema(description = "The Id of the wenet app.", example = "aisfuh9s8fnkdfhg9d8fgkdjfgnhkduyfgidjkfgkdh")
	public String appToken;

	/**
	 * The URL to post the messages for the application.
	 */
	@Schema(
			description = "The endpoint responsible for receiving the messages generated by the wenet platform for the users.",
			example = "https://app.endpoint.com/messages")
	public String messageCallbackUrl;

	/**
	 * The metadata of the application.
	 */
	@Schema(type = "object", description = "App metadata (such as its name and description).")
	@JsonDeserialize(using = JsonObjectDeserializer.class)
	public Object metadata;

	/**
	 * The platform that the application is allowed to use.
	 */
	@ArraySchema(
			schema = @Schema(type = "object"),
			arraySchema = @Schema(description = "The allowed platform for the application."))
	@JsonDeserialize(contentUsing = JsonObjectDeserializer.class)
	public List<Object> allowedPlatforms;

}