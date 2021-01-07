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

import java.util.List;

import eu.internetofus.common.components.profile_manager.WeNetProfileManager;
import eu.internetofus.common.components.profile_manager.WeNetUserProfile;
import eu.internetofus.common.vertx.ModelContext;
import eu.internetofus.common.vertx.ModelResources;
import eu.internetofus.common.vertx.ServiceContext;
import eu.internetofus.wenet_interaction_protocol_engine.persistence.NormsRepository;
import io.vertx.core.AsyncResult;
import io.vertx.core.Handler;
import io.vertx.core.Vertx;
import io.vertx.core.json.JsonObject;
import io.vertx.ext.web.api.service.ServiceRequest;
import io.vertx.ext.web.api.service.ServiceResponse;

/**
 * Implements the services defined in the {@link Norms}.
 *
 * @author UDT-IA, IIIA-CSIC
 */
public class NormsResource implements Norms {

  /**
   * The event bus that is using.
   */
  protected Vertx vertx;

  /**
   * The repository to manage the norms.
   */
  protected NormsRepository repository;

  /**
   * The manager to manage the users profile.
   */
  protected WeNetProfileManager profileManager;

  /**
   * Create a new instance to provide the services of the {@link Norms}.
   *
   * @param vertx where resource is defined.
   */
  public NormsResource(final Vertx vertx) {

    this.vertx = vertx;
    this.repository = NormsRepository.createProxy(vertx);
    this.profileManager = WeNetProfileManager.createProxy(vertx);
  }

  /**
   * Create the profile context.
   *
   * @return the context of the {@link WeNetUserProfile}.
   */
  protected ModelContext<PublishedNorm, String> createPublishedNormContext() {

    final var context = new ModelContext<PublishedNorm, String>();
    context.name = "publishedNorm";
    context.type = PublishedNorm.class;
    return context;

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void publishNorm(final JsonObject body, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.createModel(this.vertx, body, model, this.repository::storePublishedNorm, context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNormsPage(final String name, final String description, final List<String> keywords, final String publisherId, final Long publishFrom, final Long publishTo, final List<String> order, final int offset,
      final int limit, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var context = new ServiceContext(request, resultHandler);
    ModelResources.retrieveModelsPage(offset, limit, (page, promise) -> {

      page.query = NormsRepository.createPublishedNormsPageQuery(name, description, keywords, publisherId, publishFrom, publishTo);
      page.sort = NormsRepository.createPublishedNormsPageSort(order);
      this.repository.retrievePublishedNormsPageObject(page, search -> promise.handle(search));

    }, context);
  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void retrievePublishedNorm(final String publishedNormId, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.retrieveModel(model, this.repository::searchPublishedNorm, context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void updatePublishedNorm(final String publishedNormId, final JsonObject body, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.updateModel(this.vertx, body, model, this.repository::searchPublishedNorm, this.repository::updatePublishedNorm, context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void deletePublishedNorm(final String publishedNormId, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.deleteModel(model, this.repository::deletePublishedNorm, context);

  }

  /**
   * {@inheritDoc}
   */
  @Override
  public void mergePublishedNorm(final String publishedNormId, final JsonObject body, final ServiceRequest request, final Handler<AsyncResult<ServiceResponse>> resultHandler) {

    final var model = this.createPublishedNormContext();
    model.id = publishedNormId;
    final var context = new ServiceContext(request, resultHandler);
    ModelResources.mergeModel(this.vertx, body, model, this.repository::searchPublishedNorm, this.repository::updatePublishedNorm, context);

  }

}
