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

package eu.internetofus.wenet_interaction_protocol_engine.api.norms;

import static eu.internetofus.common.components.MergesTest.assertCanMerge;
import static eu.internetofus.common.components.MergesTest.assertCannotMerge;
import static eu.internetofus.common.components.UpdatesTest.assertCanUpdate;
import static eu.internetofus.common.components.UpdatesTest.assertCannotUpdate;
import static eu.internetofus.common.components.ValidationsTest.assertIsNotValid;
import static eu.internetofus.common.components.ValidationsTest.assertIsValid;
import static org.assertj.core.api.Assertions.assertThat;

import eu.internetofus.common.components.Model;
import eu.internetofus.common.components.ModelTestCase;
import eu.internetofus.common.components.StoreServices;
import eu.internetofus.common.components.ValidationsTest;
import eu.internetofus.common.components.profile_manager.NormTest;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.wenet_interaction_protocol_engine.WeNetInteractionProtocolEngineIntegrationExtension;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Future;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.junit5.VertxTestContext;
import java.util.ArrayList;
import java.util.UUID;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.api.extension.ExtendWith;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

/**
 * Test the {@link PublishedNorm}
 *
 * @see PublishedNorm
 *
 *
 * @author UDT-IA, IIIA-CSIC
 */
@ExtendWith(WeNetInteractionProtocolEngineIntegrationExtension.class)
public class PublishedNormTest extends ModelTestCase<PublishedNorm> {

  /**
   * {@inheritDoc}
   */
  @Override
  public PublishedNorm createModelExample(final int index) {

    final var model = new PublishedNorm();
    model.name = "Published norm " + index;
    model.description = "Description of published norm " + index;
    model.keywords = new ArrayList<>();
    for (var i = index - 2; i < index + 2; i++) {

      model.keywords.add("keyword " + i);
    }
    model.publisherId = "Published_identifier_" + index;
    model.norm = new NormTest().createModelExample(index);
    model.norm.id = "norm_" + index;

    return model;
  }

  /**
   * Create an example model that has the specified index.
   *
   * @param index         to use in the example.
   * @param vertx         event bus to use.
   * @param testContext   test context to use.
   * @param createHandler the component that will manage the created model.
   */
  public void createModelExample(final int index, final Vertx vertx, final VertxTestContext testContext,
      final Handler<AsyncResult<PublishedNorm>> createHandler) {

    StoreServices.storeProfile(new WeNetUserProfile(), vertx, testContext).onSuccess(stored -> {

      final var model = this.createModelExample(index);
      model.publisherId = stored.id;
      createHandler.handle(Future.succeededFuture(model));

    });

  }

  /**
   * Check that the {@link #createModelExample(int)} is valid.
   *
   * @param index       to verify
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @ParameterizedTest(name = "The model example {0} has to be valid")
  @ValueSource(ints = { 0, 1, 2, 3, 4, 5 })
  public void shouldExampleNotBeValid(final int index, final Vertx vertx, final VertxTestContext testContext) {

    final var model = this.createModelExample(index);
    assertIsNotValid(model, "publisherId", vertx, testContext);

  }

  /**
   * Check that the
   * {@link #createModelExample(int, Vertx, VertxTestContext, Handler)} is valid.
   *
   * @param index       to verify
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @ParameterizedTest(name = "The model example {0} has to be valid")
  @ValueSource(ints = { 0, 1, 2, 3, 4, 5 })
  public void shouldExampleBeValid(final int index, final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(index, vertx, testContext,
        testContext.succeeding(model -> assertIsValid(model, vertx, testContext)));

  }

  /**
   * Check a model with an identifier is valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldBeValidWithId(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.id = UUID.randomUUID().toString();
      assertIsValid(model, vertx, testContext);

    }));

  }

  /**
   * Check a model with a duplicated identifier is noit valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldMotBeValidWithDuplicatedId(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      NormsRepository.createProxy(vertx).storePublishedNorm(model, testContext.succeeding(stored -> {

        model.id = stored.id;
        assertIsNotValid(model, "id", vertx, testContext);

      }));

    }));

  }

  /**
   * Check that a model with a large name is not valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldModelWithLargeNameNotBeValid(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.name = ValidationsTest.STRING_256;
      assertIsNotValid(model, "name", vertx, testContext);
    }));

  }

  /**
   * Check that a model with a large description is not valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldModelWithLargeDescriptionNotBeValid(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.description = ValidationsTest.STRING_1024;
      assertIsNotValid(model, "description", vertx, testContext);
    }));

  }

  /**
   * Check that a model with a large keyword is not valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldModelWithLargeKeywordNotBeValid(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.keywords.add(3, ValidationsTest.STRING_256);
      assertIsNotValid(model, "keywords[3]", vertx, testContext);
    }));

  }

  /**
   * Check that a model without a norm is not valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldModelWithLargeNormNotBeValid(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.norm = null;
      assertIsNotValid(model, "norm", vertx, testContext);
    }));

  }

  /**
   * Check that a model with an undefined published is not valid.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see WeNetUserProfile#validate(String, Vertx)
   */
  @Test
  public void shouldModelWithUndefinedPublishedNotBeValid(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(model -> {

      model.publisherId = "undefined";
      assertIsNotValid(model, "publisherId", vertx, testContext);
    }));

  }

  /**
   * Check that merge {@code null} value.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldMergeNullValue(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      assertCanMerge(target, null, vertx, testContext, merged -> testContext.verify(() -> {

        assertThat(merged).isSameAs(target);

      }));
    }));

  }

  /**
   * Check that merge examples.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldMergeExamples(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      this.createModelExample(2, vertx, testContext, testContext.succeeding(source -> {

        assertCanMerge(target, source, vertx, testContext, merged -> testContext.verify(() -> {

          assertThat(merged).isNotEqualTo(target);
          source._creationTs = merged._creationTs;
          source._lastUpdateTs = merged._lastUpdateTs;
          source.norm.id = merged.norm.id;
          assertThat(merged).isEqualTo(source);

        }));
      }));
    }));

  }

  /**
   * Check not merge with invalid name.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotMergeWithABadName(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = new PublishedNorm();
      source.name = ValidationsTest.STRING_256;
      assertCannotMerge(target, source, "name", vertx, testContext);
    }));

  }

  /**
   * Check not merge with invalid description.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotMergeWithABadDescription(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = new PublishedNorm();
      source.description = ValidationsTest.STRING_1024;
      assertCannotMerge(target, source, "description", vertx, testContext);
    }));

  }

  /**
   * Check not merge with invalid keyword.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotMergeWithABadKeyword(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = new PublishedNorm();
      source.keywords = new ArrayList<>();
      source.keywords.add(" key1 ");
      source.keywords.add(null);
      source.keywords.add("");
      source.keywords.add(ValidationsTest.STRING_256);
      assertCannotMerge(target, source, "keywords[3]", vertx, testContext);
    }));

  }

  /**
   * Check not merge with undefined publisher.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#merge(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotMergeWithAnUndefinedPublishedId(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = new PublishedNorm();
      source.publisherId = "undefined";
      assertCannotMerge(target, source, "publisherId", vertx, testContext);
    }));

  }

  /**
   * Check that update {@code null} value.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldUpdateNullValue(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      assertCanUpdate(target, null, vertx, testContext, updated -> testContext.verify(() -> {

        assertThat(updated).isSameAs(target);

      }));
    }));

  }

  /**
   * Check that update examples.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldUpdateExamples(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      this.createModelExample(2, vertx, testContext, testContext.succeeding(source -> {

        assertCanUpdate(target, source, vertx, testContext, updated -> testContext.verify(() -> {

          assertThat(updated).isNotEqualTo(target);
          source._creationTs = updated._creationTs;
          source._lastUpdateTs = updated._lastUpdateTs;
          source.norm.id = updated.norm.id;
          assertThat(updated).isEqualTo(source);

        }));
      }));
    }));

  }

  /**
   * Check not update with invalid name.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotUpdateWithABadName(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = Model.fromJsonObject(target.toJsonObject(), PublishedNorm.class);
      source.name = ValidationsTest.STRING_256;
      assertCannotUpdate(target, source, "name", vertx, testContext);
    }));

  }

  /**
   * Check not update with invalid description.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotUpdateWithABadDescription(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = Model.fromJsonObject(target.toJsonObject(), PublishedNorm.class);
      source.description = ValidationsTest.STRING_1024;
      assertCannotUpdate(target, source, "description", vertx, testContext);
    }));

  }

  /**
   * Check not update with invalid keyword.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotUpdateWithABadKeyword(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = Model.fromJsonObject(target.toJsonObject(), PublishedNorm.class);
      source.keywords = new ArrayList<>();
      source.keywords.add(" key1 ");
      source.keywords.add(null);
      source.keywords.add("");
      source.keywords.add(ValidationsTest.STRING_256);
      assertCannotUpdate(target, source, "keywords[3]", vertx, testContext);
    }));

  }

  /**
   * Check not update with undefined publisher.
   *
   * @param vertx       event bus to use.
   * @param testContext context to test.
   *
   * @see PublishedNorm#update(PublishedNorm, String, Vertx)
   */
  @Test
  public void shouldNotUpdateWithAnUndefinedPublishedId(final Vertx vertx, final VertxTestContext testContext) {

    this.createModelExample(1, vertx, testContext, testContext.succeeding(target -> {

      final var source = Model.fromJsonObject(target.toJsonObject(), PublishedNorm.class);
      source.publisherId = "undefined";
      assertCannotUpdate(target, source, "publisherId", vertx, testContext);
    }));

  }
}
