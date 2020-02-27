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

package eu.internetofus.wenet_interaction_protocol_engine;

import java.io.InputStream;

import org.apache.commons.io.IOUtils;
import org.apache.commons.lang3.builder.EqualsBuilder;
import org.apache.commons.lang3.builder.HashCodeBuilder;
import org.apache.commons.lang3.builder.ToStringBuilder;
import org.apache.commons.lang3.builder.ToStringStyle;

import com.fasterxml.jackson.annotation.JsonInclude;
import com.fasterxml.jackson.annotation.JsonInclude.Include;

import io.vertx.core.buffer.Buffer;
import io.vertx.core.json.Json;
import io.vertx.core.json.JsonObject;

/**
 * Define a data model.
 *
 * @author UDT-IA, IIIA-CSIC
 */
@JsonInclude(Include.NON_EMPTY)
public class Model {

	/**
	 * Reflections equals.
	 *
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#equals(java.lang.Object)
	 * @see EqualsBuilder#reflectionEquals(Object, Object,String...)
	 */
	@Override
	public boolean equals(Object obj) {

		return EqualsBuilder.reflectionEquals(this, obj);

	}

	/**
	 * Reflection hash code.
	 *
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#hashCode()
	 * @see HashCodeBuilder#reflectionHashCode(Object, String...)
	 */
	@Override
	public int hashCode() {

		return HashCodeBuilder.reflectionHashCode(this);

	}

	/**
	 * {@inheritDoc}
	 *
	 * @see java.lang.Object#toString()
	 * @see ToStringBuilder#reflectionToString(Object)
	 */
	@Override
	public String toString() {

		return ToStringBuilder.reflectionToString(this, ToStringStyle.SHORT_PREFIX_STYLE);

	}

	/**
	 * Obtain the model form the string representation.
	 *
	 * @param value to obtain the model.
	 * @param type  of model to obtain
	 * @param <T>   to obtain
	 *
	 * @return the model coded on JSON or {@code null} if can not obtain it.
	 */
	public static <T extends Model> T fromString(String value, Class<T> type) {

		try {

			return Json.decodeValue(value, type);

		} catch (final Throwable throwable) {

			return null;
		}
	}

	/**
	 * Obtain the model associated to a {@link JsonObject}.
	 *
	 * @param value object to obtain the model.
	 * @param type  of model to obtain
	 * @param <T>   to obtain
	 *
	 * @return the model defined on the object or {@code null} if can not obtain it.
	 */
	public static <T extends Model> T fromJsonObject(JsonObject value, Class<T> type) {

		try {

			return Json.decodeValue(value.toBuffer(), type);

		} catch (final Throwable throwable) {

			return null;
		}
	}

	/**
	 * Convert a model to JSON string.
	 *
	 * @return the string representation of this model in JSON or {@code null} if it
	 *         can not convert it.
	 */
	public String toJsonString() {

		try {

			return Json.encode(this);

		} catch (final Throwable throwable) {

			return null;
		}

	}

	/**
	 * Convert a model to a {@link JsonObject}.
	 *
	 * @return the object of the model or {@code null} if can not convert it.
	 */
	public JsonObject toJsonObject() {

		try {

			return new JsonObject(this.toBuffer());

		} catch (final Throwable throwable) {

			return null;
		}

	}

	/**
	 * Convert this model to a buffer.
	 *
	 * @return the buffer that contains the JSON encoding of this model or
	 *         {@code null} if can not convert it.
	 */
	public Buffer toBuffer() {

		try {

			return Json.encodeToBuffer(this);

		} catch (final Throwable throwable) {

			return null;
		}
	}

	/**
	 * Obtain the model associated to a {@link Buffer}.
	 *
	 * @param buffer to obtain the model.
	 * @param type   of model to obtain
	 * @param <T>    to obtain
	 *
	 * @return the model defined on the buffer or {@code null} if can not obtain it.
	 */
	public static <T extends Model> T fromBuffer(Buffer buffer, Class<T> type) {

		try {

			return Json.decodeValue(buffer, type);

		} catch (final Throwable throwable) {

			return null;
		}
	}

	/**
	 * Obtain the model associated to a resource.
	 *
	 * @param resourceName where the model to obtain is defined In JSON.
	 * @param type         of model to obtain
	 * @param <T>          model to obtain
	 *
	 * @return the model defined on the resource or {@code null} if can not obtain
	 *         it.
	 */
	public static <T extends Model> T loadFromResource(String resourceName, Class<T> type) {

		try {

			final InputStream stream = type.getClassLoader().getResourceAsStream(resourceName);
			final String encoded = IOUtils.toString(stream);
			return fromString(encoded, type);

		} catch (final Throwable throwable) {

			return null;
		}

	}

}
